import Config

import Network.SimpleIRC

main = do
    (configdir,fullfilelist) <- initConfig
    let files = map ((configdir++"/")++) $ dropConfigs fullfilelist
    putStr "Opening: "
    putStrLn $ show configdir
    putStr "Connecting to: "
    putStrLn $ show files
    configs <- mapM (readConfig) files
    mapM (\net -> connect net True False) $ drop 1 configs
    connect (configs!!0) False True
