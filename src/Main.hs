{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Concurrent
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.SQLite
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.UTF8 (toString)
import Data.Char
import Data.List hiding (insert)
import Data.Maybe
import Data.Monoid
import Data.Word
import Network.DNS hiding (lookup)
import Network.Socket.ByteString
import Network.Socket hiding (recvFrom, sendTo)
import System.Environment

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
	[persistLowerCase|
	Name
		name String
		rr_a String Maybe
		rr_aaaa String Maybe
		UniqueName name
	|]

type Ddns = ExceptT String (ReaderT SQLiteHandle IO)

runDdns :: SQLiteHandle -> Ddns a -> IO (Either String a)
runDdns sqlh = flip runReaderT sqlh . runExceptT

execSql :: String -> Ddns [[Row Value]]
execSql statement = do
	sqlh <- ask
	ExceptT . liftIO $ execStatement sqlh statement

execSqlParam :: String -> [(String, Value)] -> Ddns [[Row Value]]
execSqlParam statement param = do
	sqlh <- ask
	ExceptT . liftIO $ execParamStatement sqlh statement param

newName :: String -> Ddns Bool
newName domain = do
			execSqlParam
				"insert into names (name) values (:name);"
				[(":name", Text domain)]
			return True

updateRecord :: String -> TYPE -> RData -> Ddns ()
updateRecord domain qtype rrdata = do
	void $ execSqlParam
		( intercalate " "
			[ "insert or replace into rr_a"
			, "(names_id, ip)"
			, "values"
			, "  ( (select names_id from names where name='example')"
			, "  , :data"
			, "  );"
			]
		)
		[(":data", Text . show $ rrdata)]

queryDomain :: String -> TYPE -> Ddns RData
queryDomain domain qtype = query qtype >>= extract >>= wrap qtype
	where
		query :: TYPE -> Ddns [[Row Value]]
		query A = execSqlParam "select rr_a.ip from names join rr_a using (names_id) where names.name = :name;" [(":name", Text domain)]
		query AAAA = execSqlParam "select rr_aaaa.ip from names join rr_aaaa using (names_id) where names.name = :name;" [(":name", Text domain)]
		query _ = throwE "unsupported type"

		extract :: [[Row Value]] -> Ddns String
		extract [[[("ip", Text ip)]]] = return ip
		extract _ = throwE "does not exist"

		wrap :: TYPE -> String -> Ddns RData
		wrap A s = return . RD_A . read $ s
		wrap AAAA s = return . RD_AAAA . read $ s
		wrap _ _ = throwE "unsupported type"

main :: IO ()
main = runSqlite "t.db" $ do
	runMigration migrateAll
	insert $ Name "example" Nothing Nothing
	return ()
{-
main = run >>= \case
		Left err -> putStrLn $ "[e] fatal! " ++ err
		_ -> return ()
-}

run :: IO (Either String ())
run = do
	putStrLn $ "[i] ddns starting"
	env <- getEnvironment
	let
		port = fromMaybe 53 $ do
			portStr <- lookup "DDNS_PORT" env
			portWord <- readMay portStr :: Maybe Word16
			return $ fromIntegral portWord

		sqliteDb = fromMaybe "names.db" $ lookup "DDNS_DB" env

		suffix = fromMaybe ".example.com." $ do
			lookup "DDNS_SUFFIX" env

	putStrLn $ "[i] config:"
	putStrLn $ "[i]   DDNS_PORT = " ++ show port
	putStrLn $ "[i]   DDNS_DB = " ++ show sqliteDb
	putStrLn $ "[i]   DDNS_SUFFIX = " ++ show suffix

	sqlh <- openConnection sqliteDb

	runDdns sqlh $ do
		execSql "PRAGMA foreign_keys;"

		execSql $ intercalate " "
			[ "create table if not exists names"
			, "( name text primary key"
			, ");"
			]

		execSql $ intercalate " "
			[ "create table if not exists rr_a"
			, "( name text primary key"
			, ", ip text not null"
			, ", foreign key(name) references names(name)"
			, ");"
			]

		execSql $ intercalate " "
			[ "create table if not exists rr_aaaa"
			, "( name text primary key"
			, ", ip text not null"
			, ", foreign key(name) references names(name)"
			, ");"
			]

		newName "example"
		updateRecord "example" A (RD_A . read $ "1.2.3.4")


		s <- liftIO $ socket AF_INET Datagram defaultProtocol
		liftIO . bind s $ SockAddrInet port iNADDR_ANY

		forever $ do
			(query, rxSock) <- liftIO $ recvFrom s 512
			res <- runExceptT $ do
				DNSMessage{..} <- hoistEither . decode . fromStrict $ query
				answers <- liftIO . forM question $ \q@Question{..} -> runDdns sqlh $ do
					let (name, thisSuffix) = span (/= '.') $ toString qname

					tryAssert "unsupported type" (qtype == A)
					tryAssert "bad suffix" (thisSuffix == suffix)

					x <- queryDomain name qtype

					return ResourceRecord
						{ rrname = qname
						, rrtype = A
						, rrttl = 600
						, rdata = x
						}

				forM_ (zip question answers) $ \(Question{..}, a) ->
					either
						(\reason -> liftIO . putStrLn $ "[e] >> " ++ show qtype ++ " " ++ show qname ++ " --> FAIL: " ++ reason)
						(\ResourceRecord{..} -> liftIO . putStrLn $ "[i] >> " ++ show qtype ++ " " ++ show qname ++ " --> " ++ show rdata)
						a
				
				let
					response = DNSMessage
						{ header = DNSHeader
							{ identifier = identifier header
							, flags = DNSFlags
								{ qOrR = QR_Response
								, opcode = opcode . flags $ header
								, authAnswer = True
								, trunCation = False
								, recDesired = False
								, recAvailable = False
								, rcode = NoErr
								}
							}
						, question = []
						, answer = []
						, authority = rights answers
						, additional = []
						}

				liftIO $ sendTo s (toStrict $ encode response) rxSock
			case res of
				Left e -> liftIO $ putStrLn $ "[e] " ++ e
				_ -> return ()
