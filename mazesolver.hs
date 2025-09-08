-- maze-solver.hs
import qualified Data.Set as Set

-- Tipovi podataka
type Position = (Int, Int)
type Maze = [[Cell]]

data Cell = Wall | Empty | Start | Exit | Path
    deriving (Eq, Show)

-- Predefinisani lavirinti
simpleMaze :: Maze
simpleMaze =
    [[Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
     [Wall, Start, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Wall],
     [Wall, Wall, Wall, Empty, Wall, Empty, Wall, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Wall, Empty, Empty, Wall, Empty, Wall],
     [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall],
     [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Exit, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]]

mediumMaze :: Maze
mediumMaze =
    [[Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
     [Wall, Start, Empty, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Wall],
     [Wall, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Wall, Empty, Empty, Wall],
     [Wall, Empty, Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall],
     [Wall, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall],
     [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall, Empty, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Exit, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]]

complexMaze :: Maze
complexMaze =
    [[Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
     [Wall, Start, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall],
     [Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall, Wall, Wall, Wall, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall],
     [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall, Empty, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Wall],
     [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall],
     [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Exit, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]]

-- Pomocne funkcije
renderCell :: Cell -> String
renderCell Wall = "##"
renderCell Empty = "  "
renderCell Start = "SS"
renderCell Exit = "EE"
renderCell Path = ".."

renderMaze :: Maze -> String
renderMaze maze = unlines $ map renderRow maze
  where
    renderRow = concatMap renderCell

setCell :: Position -> Cell -> Maze -> Maze
setCell (x, y) cell maze =
    take y maze ++ 
    [take x (maze !! y) ++ [cell] ++ drop (x + 1) (maze !! y)] ++
    drop (y + 1) maze

getCell :: Position -> Maze -> Cell
getCell (x, y) maze = maze !! y !! x

isValidPosition :: Position -> Int -> Int -> Bool
isValidPosition (x, y) width height =
    x >= 0 && x < width && y >= 0 && y < height

-- BFS algoritam za resavanje
solveBFS :: Maze -> Maybe [Position]
solveBFS maze = 
    let start = findStart maze
    in bfs maze [([start], start)] (Set.singleton start)
  where
    findStart :: Maze -> Position
    findStart maze = head [(x, y) | y <- [0..length maze - 1],
                                   x <- [0..length (head maze) - 1],
                                   getCell (x, y) maze == Start]

    isExit :: Cell -> Bool
    isExit Exit = True
    isExit _ = False

    bfs :: Maze -> [([Position], Position)] -> Set.Set Position -> Maybe [Position]
    bfs _ [] _ = Nothing
    bfs maze ((path, current):queue) visited =
        if isExit (getCell current maze)
        then Just (reverse path)
        else let neighbors = getValidNeighbors maze current visited
                 newPaths = map (\pos -> (pos:path, pos)) neighbors
                 newQueue = queue ++ newPaths
                 newVisited = foldl (\acc pos -> Set.insert pos acc) visited neighbors
             in bfs maze newQueue newVisited

    getValidNeighbors :: Maze -> Position -> Set.Set Position -> [Position]
    getValidNeighbors maze (x, y) visited =
        let neighbors = [(x, y-1), (x, y+1), (x+1, y), (x-1, y)]
            width = length (head maze)
            height = length maze
        in filter (\pos@(nx, ny) -> 
            isValidPosition pos width height &&
            getCell pos maze /= Wall &&
            not (Set.member pos visited)) neighbors

-- DFS algoritam (alternativa)
solveDFS :: Maze -> Maybe [Position]
solveDFS maze = 
    let start = findStart maze
    in dfs maze [start] Set.empty
  where
    findStart :: Maze -> Position
    findStart maze = head [(x, y) | y <- [0..length maze - 1],
                                   x <- [0..length (head maze) - 1],
                                   getCell (x, y) maze == Start]

    isExit :: Cell -> Bool
    isExit Exit = True
    isExit _ = False

    dfs :: Maze -> [Position] -> Set.Set Position -> Maybe [Position]
    dfs maze (current:path) visited
        | isExit (getCell current maze) = Just (reverse (current:path))
        | otherwise =
            let neighbors = getValidNeighbors maze current visited
            in case neighbors of
                (next:_) -> dfs maze (next:current:path) (Set.insert next visited)
                [] -> case path of
                    [] -> Nothing
                    (prev:rest) -> dfs maze (prev:rest) visited

    getValidNeighbors :: Maze -> Position -> Set.Set Position -> [Position]
    getValidNeighbors maze (x, y) visited =
        let neighbors = [(x, y-1), (x, y+1), (x+1, y), (x-1, y)]
            width = length (head maze)
            height = length maze
        in filter (\pos@(nx, ny) -> 
            isValidPosition pos width height &&
            getCell pos maze /= Wall &&
            not (Set.member pos visited)) neighbors

-- Vizualizacija resenja
renderSolution :: Maze -> [Position] -> String
renderSolution maze path = 
    let mazeWithPath = foldl (\m pos -> setCell pos Path m) maze path
    in renderMaze mazeWithPath

-- Glavna funkcija
main :: IO ()
main = do
    putStrLn "Lavirint Solver"
    putStrLn "==============="
    
    -- Izaberi lavirint
    let selectedMaze = complexMaze
    
    putStrLn "Lavirint:"
    putStrLn $ renderMaze selectedMaze
    
    putStrLn "Trazim put BFS algoritmom..."
    case solveBFS selectedMaze of
        Just path -> do
            putStrLn "Put pronadjen (BFS)!"
            putStrLn $ "Duzina puta: " ++ show (length path)
            putStrLn "Resenje:"
            putStrLn $ renderSolution selectedMaze path
        
        Nothing -> do
            putStrLn "BFS nije pronasao put!"
            putStrLn "Pokusavam DFS algoritmom..."
            case solveDFS selectedMaze of
                Just path -> do
                    putStrLn "Put pronadjen (DFS)!"
                    putStrLn $ "Duzina puta: " ++ show (length path)
                    putStrLn "Resenje:"
                    putStrLn $ renderSolution selectedMaze path
                Nothing -> putStrLn "Nema resenja ni sa DFS!"

-- Funkcija za prikaz razlicitih lavirinata
showAllMazes :: IO ()
showAllMazes = do
    putStrLn "Simple Maze:"
    putStrLn $ renderMaze simpleMaze
    putStrLn "\nMedium Maze:"
    putStrLn $ renderMaze mediumMaze
    putStrLn "\nComplex Maze:"
    putStrLn $ renderMaze complexMaze

-- Test funkcija
testMaze :: IO ()
testMaze = do
    putStrLn "Test lavirint:"
    putStrLn $ renderMaze simpleMaze
    putStrLn $ "Start pozicija: " ++ show (findStart simpleMaze)
    where findStart maze = head [(x, y) | y <- [0..length maze - 1],
                                        x <- [0..length (head maze) - 1],
                                        getCell (x, y) maze == Start]


{-
pokretanje
> main
> showAllMazes
> testMaze
> solveBFS simpleMaze
> solveDFS mediumMaze
-}