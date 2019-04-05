{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Process
import System.IO
import System.Directory



data Arguments = Arguments {
    auction_id :: [String]
    ,access_token :: String
} deriving (Show, Data, Typeable)

arguments = Arguments {
    auction_id = def &= typ "fbid" &= help "Dynasty Baseball Group Auction IDs"
    ,access_token = def &= typ "Access Token" &= help "Facebook User Access Token"
} &= summary "Batch Get Auction Results. Get access token here: https://developers.facebook.com/tools/explorer/"

activate_source = "source ~/Code/Virtualenv/bin/activate"
fiancial_transactions = "python /Users/Varun/Code/Auction_Calculator/get_financial_transactions.py"
auction_results = "python /Users/Varun/Code/Auction_Calculator/get_auction.py"


downloadTransactions :: String -> IO ()
downloadTransactions accessToken = callCommand $ activate_source 
    ++ "; "
    ++ fiancial_transactions
    ++ " "
    ++ accessToken

getAuction :: String -> String -> IO ()
getAuction accessToken auctionID = callCommand $ activate_source 
    ++ "; "
    ++ auction_results
    ++ " "
    ++ auctionID
    ++ " "
    ++ accessToken

getAllAuctions :: String -> [String] -> IO()
getAllAuctions accessToken [] = return ()
getAllAuctions accessToken auctionIDs = do
    let (auctionID, otherIDs) = (head auctionIDs, tail auctionIDs)
    getAuction accessToken auctionID
    renameFile "input.csv" (auctionID ++ ".csv")
    getAllAuctions accessToken otherIDs

main :: IO ()
main = do
    args <- cmdArgs arguments
    let accessToken = access_token args
    let auctionIDs = auction_id args
    downloadTransactions accessToken
    getAllAuctions accessToken auctionIDs
    
