
data Tree a = Node a (Tree a) (Tree a)
            | Empty
                deriving (Show)

simpleTree = Node "parent"
                (Node "left child" Empty Empty)
                (Node "right child" Empty Empty)

data Tree' a = Node' (Maybe a) (Maybe (Tree' a)) (Maybe (Tree' a)) deriving (Show)

simpleTree' = Node' (Just "parent")
                (Just (Node' (Just "left child") Nothing Nothing))
                (Just (Node' (Just "right child") Nothing Nothing))
