{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Pipes
import qualified Pipes.Prelude as P
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Prelude hiding (putStrLn)


-- 顧客情報のデータ型を定義
data Customer = Customer
  { name     :: Text
  , email    :: Text
  , category :: Text
  } deriving (Show)

instance FromNamedRecord Customer where
  parseNamedRecord r = Customer <$> r .: "name" <*> r .: "email" <*> r .: "category"

-- 顧客のタイプごとのアクションを表すデータ型
data CustomerAction next
  = LogVIP Customer next
  | LogRegular Customer next
  | LogOccasional Customer next
  deriving (Functor)  -- Derive Functorインスタンス

-- Freeモナドを使った顧客タイプ別のログ関数
logCustomer :: Customer -> Free CustomerAction ()
logCustomer cust = case category cust of
    "A" -> liftF (LogVIP cust ())
    "B" -> liftF (LogRegular cust ())
    "C" -> liftF (LogOccasional cust ())
    _   -> return () -- 不明なカテゴリはスキップ

-- 各顧客タイプごとのログアクションを実行するインタプリタ
interpretAction :: MonadIO m => CustomerAction x -> m x
interpretAction (LogVIP cust next) = do
    liftIO $ putStrLn $ "VIP Customer: " <> name cust <> " (" <> email cust <> ")"
    return next  -- nextを呼び出して次のアクションを実行
interpretAction (LogRegular cust next) = do
    liftIO $ putStrLn $ "Regular Customer: " <> name cust <> " (" <> email cust <> ")"
    return next
interpretAction (LogOccasional cust next) = do
    liftIO $ putStrLn $ "Occasional Customer: " <> name cust <> " (" <> email cust <> ")"
    return next


-- Freeモナドを実行するためのヘルパー関数
runCustomerLog :: MonadIO m => Free CustomerAction () -> m ()
runCustomerLog = foldFree interpretAction

-- 顧客をストリームとして処理する
streamCustomers :: Monad m => V.Vector Customer -> Producer Customer m ()
streamCustomers customers = each (V.toList customers)

-- メイン処理：CSVを読み込んで顧客のログアクションを実行
main :: IO ()
main = do
    -- CSVファイルを読み込む
    csvData <- BL.readFile "C:\\Users\\thainguyen\\Resources\\Haskell\\Haskell-projects\\Internet_Customer_Processor\\app\\customers.csv"
    case decodeByName csvData of
        Left err -> putStrLn $ T.pack err
        Right (_, customers) -> do
            -- 顧客のストリームを生成
            runEffect $ for (streamCustomers customers) $ \cust -> do
                -- Freeモナドで顧客のログアクションを定義し、runCustomerLogで実行
                runCustomerLog (logCustomer cust)
