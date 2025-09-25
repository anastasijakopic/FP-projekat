import qualified Data.Set as Set
import System.IO
import Data.List (intercalate)

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

-- Poboljšana reprezentacija ćelija (bez emodžija)
renderCell :: Cell -> String
renderCell Wall = "##"
renderCell Empty = "  "
renderCell Start = "S "
renderCell Exit = "E "
renderCell Path = "oo"

-- Poboljšana funkcija za prikaz lavirinta
renderMaze :: Maze -> String
renderMaze maze = intercalate "\n" $ map renderRow maze
  where
    renderRow = concatMap renderCell

-- Pomoćne funkcije
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

-- BFS algoritam za rešavanje
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

-- Vizualizacija rešenja
renderSolution :: Maze -> [Position] -> String
renderSolution maze path = 
    let mazeWithPath = foldl (\m pos -> setCell pos Path m) maze path
    in renderMaze mazeWithPath

-- Meni za izbor lavirinta
chooseMaze :: IO Maze
chooseMaze = do
    let loop = do
            putStrLn "Izaberite lavirint:"
            putStrLn "1. Jednostavan"
            putStrLn "2. Srednji"
            putStrLn "3. Kompleksan"
            putStr "Vas izbor (1-3): "
            hFlush stdout
            choice <- getLine
            case choice of
                "1" -> return simpleMaze
                "2" -> return mediumMaze
                "3" -> return complexMaze
                _ -> do
                    putStrLn "Nevazeći izbor, pokušajte ponovo."
                    loop
    loop

-- Meni za izbor algoritma
chooseAlgorithm :: IO (String, Maze -> Maybe [Position])
chooseAlgorithm = do
    let loop = do
            putStrLn "Izaberite algoritam:"
            putStrLn "1. BFS (sirina prvo)"
            putStrLn "2. DFS (dubina prvo)"
            putStr "Vas izbor (1-2): "
            hFlush stdout
            choice <- getLine
            case choice of
                "1" -> return ("BFS", solveBFS)
                "2" -> return ("DFS", solveDFS)
                _ -> do
                    putStrLn "Nevazeci izbor, pokušajte ponovo."
                    loop
    loop

-- Glavna funkcija
main :: IO ()
main = do
    putStrLn "╔══════════════════════════╗"
    putStrLn "║      Lavirint Solver     ║"
    putStrLn "╚══════════════════════════╝"
    putStrLn ""
    
    -- Izaberi lavirint
    selectedMaze <- chooseMaze
    
    putStrLn "\nLavirint:"
    putStrLn $ renderMaze selectedMaze
    
    -- Izaberi algoritam
    (algorithmName, solver) <- chooseAlgorithm
    
    putStrLn $ "\nTrazim put pomocu " ++ algorithmName ++ " algoritma..."
    case solver selectedMaze of
        Just path -> do
            putStrLn "╔══════════════════════════╗"
            putStrLn "║       Put pronadjen!      ║"
            putStrLn "╚══════════════════════════╝"
            putStrLn $ "Duzina puta: " ++ show (length path) ++ " koraka"
            putStrLn "\nRjesenje:"
            putStrLn $ renderSolution selectedMaze path
        
        Nothing -> do
            putStrLn $ algorithmName ++ " nije pronasao put!"
            putStrLn "Pokusavam drugi algoritam..."
            let otherSolver = if algorithmName == "BFS" then solveDFS else solveBFS
            let otherAlgorithmName = if algorithmName == "BFS" then "DFS" else "BFS"
            case otherSolver selectedMaze of
                Just path -> do
                    putStrLn "╔══════════════════════════╗"
                    putStrLn "║       Put pronadjen!      ║"
                    putStrLn "╚══════════════════════════╝"
                    putStrLn $ "Duzina puta: " ++ show (length path) ++ " koraka (" ++ otherAlgorithmName ++ ")"
                    putStrLn "\nRešenje:"
                    putStrLn $ renderSolution selectedMaze path
                Nothing -> putStrLn "Nema rijesenja ni sa drugim algoritmom!"

    -- Pause so that when running the .exe by double-clicking the console
    -- window doesn't close immediately. User must press Enter to exit.
    putStrLn "\nPritisnite Enter za izlaz..."
    _ <- getLine
    return ()

-- Funkcija za prikaz različitih lavirinata
showAllMazes :: IO ()
showAllMazes = do
    putStrLn "Jednostavan Lavirint:"
    putStrLn $ renderMaze simpleMaze
    putStrLn "\nSrednji Lavirint:"
    putStrLn $ renderMaze mediumMaze
    putStrLn "\nKompleksan Lavirint:"
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