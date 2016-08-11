{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples      #-}

module Data.HashMap.Binary where

import           Control.Monad         (replicateM)
import           Data.Binary
import           Data.HashMap.Internal
import           GHC.Exts              (Int (..), indexArray#, newArray#,
                                        sizeofArray#, unsafeFreezeArray#,
                                        writeArray#, (+#), (<#))
import           GHC.Generics          (Generic)
import           GHC.ST                (ST (..), runST)

deriving instance Generic (Leaf k v)
deriving instance Generic (HashMap k v)

instance Binary a => Binary (Array a) where
    put (Array arr#) = do
        put (I# (sizeofArray# arr#))
        go 0#
      where
        go i# = case i# <# sizeofArray# arr# of
            0# -> return ()
            _  -> case indexArray# arr# i# of
                (# x #) -> put x >> go (i# +# 1#)

    get = do
        I# len# <- get
        list    <- replicateM (I# len#) get
        return $ runST $ ST $ \s# ->
            case newArray# len# undefined s# of
                (# s1#, marr# #) -> case go 0# marr# list s1# of
                    s2# -> case unsafeFreezeArray# marr# s2# of
                        (# s3#, arr# #) -> (# s3#, Array arr# #)
      where
        go _  _     []       s# = s#
        go i# marr# (x : xs) s# =
            case writeArray# marr# i# x s# of
                s'# -> go (i# +# 1#) marr# xs s'#

instance (Binary k, Binary v) => Binary (Leaf k v)
instance (Binary k, Binary v) => Binary (HashMap k v)
