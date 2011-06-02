{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import System.Directory
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    , doesFileExist
    )
import qualified Text.XML.Enumerator.Document as D
import qualified Text.XML.Enumerator.Parse as P
import qualified Control.Exception as E
import qualified Data.XML.Types as X
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.FileEmbed
import System.Process
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import System.Random (randomRIO)
import Control.Monad (replicateM)

data Instance = Instance
    { inName :: String
    , inCode :: String
    , inIp   :: String
    }
    deriving Show

configFile :: IO FilePath
configFile = do
    folder <- getAppUserDataDirectory "lambda-engine"
    createDirectoryIfMissing True folder
    return $ folder ++ "/instances.xml"

saveInstances :: [Instance] -> IO ()
saveInstances is = do
    cf <- configFile
    D.writeFile cf $ X.Document (X.Prologue [] Nothing []) (X.Element "instances" [] $ map go is) []
  where
    go i = X.NodeElement $ X.Element "instance" []
        [ elem "name" $ inName i
        , elem "code" $ inCode i
        , elem "ip" $ inIp i
        ]
    elem x y = X.NodeElement $ X.Element x [] [X.NodeContent $ X.ContentText $ T.pack y]

loadInstances :: IO [Instance]
loadInstances = do
    cf <- configFile
    e <- E.try $ D.readFile_ cf P.decodeEntities
    case e :: Either E.SomeException X.Document of
        Left e -> return []
        Right (X.Document _ (X.Element _ _ c) _) -> (return $ mapMaybe go c)
  where
    go (X.NodeElement (X.Element "instance" _ n)) = Instance
        <$> getText "name" n
        <*> getText "code" n
        <*> getText "ip"   n
    go _ = Nothing

    getText name (X.NodeElement (X.Element name' []
                    [X.NodeContent (X.ContentText t)]):_)
        | name == name' = Just $ T.unpack t
    getText name (_:xs) = getText name xs
    getText _ [] = Nothing

getInst :: String -> [Instance] -> Maybe Instance
getInst _ [] = Nothing
getInst s (i:is)
    | inName i == s = Just i
    | otherwise = getInst s is

main :: IO ()
main = do
    instances <- loadInstances

    let withInst name go =
            case getInst name instances of
                Nothing -> error $ "No such instance: " ++ name
                Just i -> go i

    args <- getArgs
    case args of
        ["init"] -> runInstance instances "default"
        ["init", name] -> runInstance instances name
        ["setup"] -> withInst "default" setup
        ["setup", name] -> withInst name setup
        ["deploy", lerball] -> withInst "default" $ deploy lerball
        _ -> error "Invalid arguments"

runInstance :: [Instance] -> String -> IO ()
runInstance is name = do
    case filter (\i -> inName i == name) is of
        [] -> return ()
        _ -> error $ "Instance name in use: " ++ name
    -- FIXME run <- runProc "ec2-run-instances" ["ami-06ad526f", "-k", "ec2-keypair", "-t", "t1.micro"]
    run <- readFile "run-results"
    code <- return $ head $ tail $ words $ head $ tail $ lines run
    desc <- runProc "ec2-describe-instances" [code]
    ip <- return $ (!! 13) $ words $ head $ tail $ lines desc
    let i = Instance name code ip
    print i
    saveInstances $ i : is
  where
    runProc x ys = do
        (_, outh, _, ph) <- runInteractiveProcess x ys Nothing Nothing
        _ <- waitForProcess ph
        bs <- S.hGetContents outh
        return $ S8.unpack bs

setup :: Instance -> IO ()
setup Instance { inIp = ip } = do
    _ <- rawSystem "scp" ["lamen-server", concat ["ubuntu@", ip, ":/home/ubuntu"]]

    (inh, outh, _, ph) <- runInteractiveProcess "ssh" ["ubuntu@" ++ ip] Nothing Nothing
    S.hPutStr inh $(embedFile "setup.sh")
    _ <- waitForProcess ph
    bs <- S.hGetContents outh
    S.putStr bs

deploy :: FilePath -> Instance -> IO ()
deploy lerball Instance { inIp = ip } = do
    name' <- randomStr
    let name = name' ++ ".tar.bz2"
    _ <- rawSystem "scp" [lerball, concat ["ubuntu@", ip, ":/home/ubuntu/lambda-engine/" ++ name]]
    _ <- rawSystem "ssh" ["ubuntu@" ++ ip, "mv", "/home/ubuntu/lambda-engine/" ++ name, "/home/ubuntu/lambda-engine/incoming/" ++ name]
    return ()
  where
    randomStr = replicateM 10 $ randomRIO ('A', 'Z')
