module FileManager where

import Hunix hiding (listFiles)
import qualified Hunix as H
import Control.Monad.IO.Class (liftIO)
import Types as T

directory="/home/mika/Downloads"


listFiles = H.listFiles directory

pcent :: Space -> Double
pcent (Space{used=u, avail=a}) = 100 * a' / (u'+a')
  where u' = fromInteger u
        a' = fromInteger a

free :: IO Space
free = freeSpace directory

