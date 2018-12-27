module Category where

import qualified GHC.Base (id,(.))
import Control.Monad ((>=>))

class Category cat where
    -- | the identity morphism
    id :: cat a a

    -- | morphism composition
    (.) :: cat b c -> cat a b -> cat a c
    
instance Category (->) where
    id = GHC.Base.id
    (.) = (GHC.Base..)
    
-- | Kleisli monad.
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

-- | Kleisli category
instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    (Kleisli f) . (Kleisli g) = Kleisli (g >=> f)  
    
newtype NatFunctor f a b =  NatFunctor { get :: f a}

instance Functor f => Category (NatFunctor f) where
    id = NatFunctor Prelude.id
    