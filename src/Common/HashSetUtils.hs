module Common.HashSetUtils
    ( hsFilterM
    ) where
import           Data.Foldable (foldrM)
import           Data.HashSet  (HashSet)
import qualified Data.HashSet  as HS
import           Data.Hashable (Hashable)


hsFilterM :: (Monad m, Eq a, Hashable a) => (a -> m Bool) -> HashSet a -> m (HashSet a)
hsFilterM pred hs
    | null hs = pure HS.empty
    | otherwise = foldrM (\value acc -> do
                                    valid <- pred value
                                    if valid
                                    then pure (HS.insert value acc)
                                    else pure acc
                            )
                                    HS.empty hs
