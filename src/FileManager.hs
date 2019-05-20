module FileManager where

import Hunix hiding (listFiles)
import qualified Hunix as H
import Control.Monad.IO.Class (liftIO)
import Types as T
import System.FilePath.Posix

import List
import Params



-- TODO check no backwards
listFiles :: [Char] -> IO [FileDetail]
listFiles path
  | beginWith "/" path || (any (beginWith "..") $ splitPath path) = f ""
  | otherwise = f path
  where f p = do
          -- print $ "directory: ||" ++ directory ++ "||"
          print $ "listing directory <" <> (directory </> p) <> ">" 
          l <- (H.listDir $ directory </> p) >>= mapM
            (\e -> do
                sz <- fsize (directory </> p </> H.path e)
                return FileDetail{ T.path = H.path e, T.filetype = H.filetype e,
                                 size = sz}
                )
          print l
          print "----"
          return $ if p /= ""
                   then FileDetail{T.path="..", T.filetype=Dir, size=0} : l
                   else l
  

pcent :: Space -> Double
pcent (Space{used=u, avail=a}) = 100 * a' / (u'+a')
  where u' = fromInteger u
        a' = fromInteger a

free :: IO Space
free = freeSpace directory
