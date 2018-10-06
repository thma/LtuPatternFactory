module Adapter where 

    backend :: c -> d
    
    adapter :: a -> b
    
    marshal :: a -> c
    
    unmarshal :: d -> b
    
    adapter :: a -> b
    adapter = unmarshal . backend . marshal
    