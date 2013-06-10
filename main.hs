import Config

import Network.SimpleIRC

main = do
    (configdir,fullfilelist) <- initConfig
    let files = map ((configdir++"/")++) $ dropConfigs fullfilelist
    putStr "Opening: "
    print configdir
    putStr "Connecting to: "
    print files
    configs <- mapM readConfig files
    mapM_ (\net -> connect net True False) $ drop 1 configs
    connect (head configs) False True
