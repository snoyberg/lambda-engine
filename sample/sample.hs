{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Environment (getArgs)

main = do
    _:portS:_ <- getArgs
    port <- return $ read portS
    run port $ const $ return $ responseLBS status200 [("content-type", "text/html")] "<html><head><link rel='stylesheet' href='/static/style.css'><title>SAMPLE</title></head><body><h1>SAMPLE APP</h1></body></html>"
