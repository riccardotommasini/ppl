module TUT20190115B where

    data Node a = Node { id :: Int,
                           datum :: a,
                           nodes :: [Int] } deriving Show

    data Graph a = Graph [Node a] deriving Show
    
    graph_lookup :: Graph a -> Int -> Maybe (Node a)
    graph_lookup (Graph []) _ = Nothing
    graph_lookup (Graph ((Node id d ns'):ns)) id' = 
            if id == id'
            then Just (Node id d ns')
            else graph_lookup (Graph ns) id'  


    -- helper (Node i d n) id = id == i

    -- helper' (Graph ns) id = filter (\n -> helper n id) ns

    -- graph_lookup' g id = case helper' g id of
    --     [] -> Nothing
    --     x:xs -> Just x

    helper g i j = case graph_lookup g i of
        Nothing -> False
        Just (Node _ _ adjs ) -> elem j adjs

    adjacents :: Graph a -> Int -> Int -> Bool
    adjacents g i j = (helper g i j) || (helper g j i)


    instance Functor (Node a) where
        fmap f (Node i v ns) = (Node i (f v) ns)

    instance Functor (Graph a) where
        fmap f (Graph ns) = Graph $ fmap (\n -> fmap f n) ns

