{-# LANGUAGE OverloadedStrings #-}
import qualified Text.XML.Enumerator.Document as D
import qualified Text.XML.Enumerator.Parse as P
import qualified Control.Exception as E
import qualified Data.XML.Types as X
import System.Directory
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    , doesFileExist
    , getDirectoryContents
    , removeFile
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , renameFile
    , doesDirectoryExist
    )
import Control.Concurrent (threadDelay)
import Safe (readMay)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Control.Monad.Trans.State (StateT (..), evalStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import System.Process
import Data.List (isSuffixOf)
import Control.Monad (when, unless, replicateM)
import qualified Control.Exception as E
import System.Random (randomRIO)

data AppDef = AppDef
    { adName :: String
    , adHost :: String
    , adPost :: Bool
    }
    deriving Show

loadAppDef :: FilePath -> IO AppDef
loadAppDef fp = do
    X.Document _ (X.Element _ _ c) _ <- D.readFile_ fp P.decodeEntities
    AppDef
        <$> getText "name" c
        <*> getText "hostname" c
        <*> return (maybe False (const True) (getElem "post" c))

data Post = Post
    { postDb :: String
    , postUser :: String
    , postPass :: String
    }
    deriving Show

data Site = Site
    { siteName :: String
    , sitePort :: Int
    , siteHost :: String
    , sitePost :: Maybe Post
    }
    deriving Show

configFile :: IO FilePath
configFile = do
    folder <- getAppUserDataDirectory "lambda-engine-server"
    createDirectoryIfMissing True folder
    return $ folder ++ "/sites.xml"

loadSites :: IO [Site]
loadSites = do
    cf <- configFile
    e <- E.try $ D.readFile_ cf P.decodeEntities
    case e :: Either E.SomeException X.Document of
        Left e -> return []
        Right (X.Document _ (X.Element _ _ c) _) -> (return $ mapMaybe go c)
  where
    go (X.NodeElement (X.Element "site" _ n)) = Site
        <$> getText "name" n
        <*> (getText "port" n >>= readMay)
        <*> getText "host"   n
        <*> fmap Just (getElem "post" n >>= goP)
    go _ = Nothing

    goP :: [X.Node] -> Maybe Post
    goP n = Post
        <$> getText "db" n
        <*> getText "user" n
        <*> getText "pass" n

getElem :: Monad m => X.Name -> [X.Node] -> m [X.Node]
getElem name [] = fail $ "elem not found: " ++ show name
getElem name (X.NodeElement (X.Element name' [] ns):_)
    | name == name' = return ns
getElem name (_:xs) = getElem name xs

getText :: Monad m => X.Name -> [X.Node] -> m String
getText name (X.NodeElement (X.Element name' []
                [X.NodeContent (X.ContentText t)]):_)
    | name == name' = return $ T.unpack t
getText name (_:xs) = getText name xs
getText name [] = fail $ "text elem not found: " ++ show name

main :: IO ()
main = do
    sites <- loadSites
    args <- getArgs
    (incoming, appfolder, nginxConf, initFolder) <-
        case args of
            [w, x, y, z] -> return (w, x, y, z)
            _ -> error "Usage: lamen-server <incoming folder> <app folder> <nginxConf> <initFolder>"
    evalStateT (watch incoming appfolder nginxConf initFolder) sites

noExceptions :: StateT [Site] IO () -> StateT [Site] IO ()
noExceptions f = StateT $ \s -> do
    e <- E.try $ runStateT f s
    case e :: Either E.SomeException ((), [Site]) of
        Left e' -> print e' >> return ((), s)
        Right ((), s') -> return ((), s')

watch :: FilePath -> FilePath -> FilePath -> FilePath -> StateT [Site] IO ()
watch incoming appfolder nginxConf initFolder =
    loop
  where
    fullPath :: FilePath -> FilePath
    fullPath fp = incoming ++ '/' : fp

    timeout :: Int
    timeout = 1 -- FIXME

    loop :: StateT [Site] IO ()
    loop = do
        contents <- liftIO $ getDirectoryContents incoming
        mapM_ (noExceptions . incoming') contents
        liftIO $ threadDelay $ 1000 * 1000 * timeout
        loop

    isTarBall :: FilePath -> Bool
    isTarBall = isSuffixOf ".tar.bz2"

    incoming' :: FilePath -> StateT [Site] IO ()
    incoming' fp
        | not $ isTarBall fp = do
            let fp' = fullPath fp
            de <- liftIO $ doesFileExist fp'
            when de $ liftIO $ removeFile $ fullPath fp
        | otherwise = noExceptions (do
            liftIO $ putStrLn $ "New app: " ++ fp
            tmp' <- liftIO getTemporaryDirectory
            let tmp = tmp' ++ "/lerball/"
            de' <- liftIO $ doesDirectoryExist tmp
            when de' $ liftIO $ removeDirectoryRecursive tmp
            liftIO $ createDirectoryIfMissing True tmp
            ph <- liftIO $ runProcess
                "tar"
                ["jxf", fullPath fp]
                (Just tmp)
                Nothing
                Nothing
                Nothing
                Nothing
            _ <- liftIO $ waitForProcess ph
            appdef <- liftIO $ loadAppDef $ tmp ++ "lambda-engine.xml"
            unless (null $ adName appdef) $ do
                let execName = tmp ++ adName appdef
                deExec <- liftIO $ doesFileExist $ execName
                when deExec $ do
                    liftIO $ rawSystem "chmod" ["+x", execName]
                    let dest = appfolder ++ '/' : (adName appdef)
                    de <- liftIO $ doesDirectoryExist dest
                    when de $ liftIO $ removeDirectoryRecursive dest
                    liftIO $ rawSystem "mv" [tmp, dest] -- renameFile doesn't work with partitions
                    addAppDef appdef
                    _ <- liftIO $ rawSystem "sudo" ["stop", serviceName $ adName appdef]
                    _ <- liftIO $ rawSystem "sudo" ["start", serviceName $ adName appdef]
                    return ()
            ) >> liftIO (removeFile $ fullPath fp)

    serviceName x = "le-app-" ++ x

    newSite :: AppDef -> StateT [Site] IO Site
    newSite ad = do
        site <- mkSite ad
        x <- checkUnique site
        if x then return site else newSite ad

    writeNginx sites = writeFile nginxConf $ concatMap (toNginx appfolder) sites
    writeSites sites = do
        cf <- configFile
        D.writeFile cf $ X.Document (X.Prologue [] Nothing []) (X.Element "sites" [] $ map toXml sites) []

    addAppDef :: AppDef -> StateT [Site] IO ()
    addAppDef ad = do
        sites <- get
        case filter (\x -> siteName x == adName ad) sites of
            [] -> do
                site <- newSite ad
                let sites' = site : sites
                liftIO $ writeNginx sites'
                _ <- liftIO $ rawSystem "sudo" ["restart", "nginx"]
                liftIO $ writeFile (concat [initFolder, "/", serviceName $ adName ad, ".conf"]) $ unlines
                    [ concat
                        [ "description \"Lambda engine app: "
                        , adName ad
                        , "\""
                        ]
                    , "start on runlevel [2345]"
                    , "stop on [!2345]"
                    , "respawn"
                    , concat
                        [ "chdir "
                        , appfolder
                        , '/' : adName ad
                        ]
                    , concat -- FIXME include options
                        [ "exec "
                        , appfolder
                        , '/' : adName ad
                        , '/' : adName ad
                        , " http://"
                        , adHost ad
                        , " "
                        , show $ sitePort site
                        ]
                    ]
                liftIO $ writeSites sites'
                put sites'
            _:_ -> return () -- FIXME check if we need to add a new database

mkSite ad = do
    port <- getPort
    post <- if adPost ad then fmap Just mkPost else return Nothing
    return $ Site (adName ad) port (adHost ad) post
  where
    getPort = do
        sites <- get
        return $ maximum (5000 : map sitePort sites) + 1
    mkPost = liftIO $ Post <$> randomStr <*> randomStr <*> randomStr
    randomStr = replicateM 10 $ randomRIO ('A', 'Z')

checkUnique site = do
    sites <- get
    return $ not $ any match sites
  where
    match s = or
        [ sitePort s == sitePort site
        -- FIXME , siteHost
        , match' (sitePost s) (sitePost site)
        ]
    match' Nothing _ = False
    match' _ Nothing = False
    match' (Just x) (Just y) = or
        [ postDb x == postDb y
        , postUser x == postUser y
        ]

toNginx :: FilePath -> Site -> String
toNginx appfolder s = unlines
    [ "server {"
    , concat
        [ "  server_name "
        , siteHost s
        , ";"
        ]
    , "  location / {"
    , concat
        [ "    proxy_pass http://127.0.0.1:"
        , show $ sitePort s
        , ";"
        ]
    , "  }"
    , "  location /static {"
    , concat
        [ "    root "
        , appfolder
        , '/' : siteName s
        , ";"
        ]
    , "    expires max;"
    , "  }"
    , "}"
    -- FIXME static.hostname?
    ]

toXml :: Site -> X.Node
toXml s = X.NodeElement $ X.Element "site" []
    [ elem "name" $ siteName s
    , elem "port" $ show $ sitePort s
    , elem "host" $ siteHost s
    , case sitePost s of
        Nothing -> X.NodeContent $ X.ContentText ""
        Just p -> X.NodeElement $ X.Element "post" []
            [ elem "db" $ postDb p
            , elem "user" $ postUser p
            , elem "pass" $ postPass p
            ]
    ]
  where
    elem x y = X.NodeElement $ X.Element x [] [X.NodeContent $ X.ContentText $ T.pack y]
