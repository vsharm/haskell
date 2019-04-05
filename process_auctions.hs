{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Process
import System.IO
import System.Directory

data Arguments = Arguments {
    auction_id :: [String]
} deriving (Show, Data, Typeable)

arguments = Arguments {
    auction_id = def &= typ "fbid" &= help "Dynasty Baseball Group Auction IDs"
} &= summary "Batch Get Auction Results. Get access token here: https://developers.facebook.com/tools/explorer/"

activate_source = "source ~/Code/Virtualenv/bin/activate"
process_auction = "python /Users/Varun/Code/Auction_Calculator/process_auctions.py"

processAuction :: IO ()
processAuction = callCommand $ activate_source 
    ++ "; "
    ++ process_auction

processAllAuctions :: [String] -> IO ()
processAllAuctions [] = return ()
processAllAuctions auctionIDs = do
    let (auctionID, otherIDs) = (head auctionIDs, tail auctionIDs)
    renameFile (auctionID ++ ".csv") "input.csv"
    processAuction
    renameFile "input.csv" (auctionID ++ ".csv")
    processAllAuctions otherIDs

main :: IO ()
main = do
    args <- cmdArgs arguments
    processAllAuctions $ auction_id args