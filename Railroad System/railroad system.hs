import System.Random
import Test.HUnit

data PriorityQueue = PQ PQNode PriorityQueue | Empty deriving (Show, Eq)
data PQNode = Node Int Int deriving (Show, Eq)

enqueue :: PQNode -> PriorityQueue -> PriorityQueue
enqueue (Node x k) Empty = (PQ (Node x k) Empty)
enqueue (Node x k) (PQ (Node h k1) q) | k<k1 = PQ (Node x k) (PQ (Node h k1) q)
									  | otherwise = PQ (Node h k1) newQ where newQ = enqueue (Node x k) q

enqueueAll :: [PQNode] -> PriorityQueue -> PriorityQueue
enqueueAll [] q = q
enqueueAll (h:t) q = enqueueAll t newQ where newQ = enqueue h q

--random network generation
getMatrix n [] = []
getMatrix n l = h:(getMatrix n t) where  (h,t)=splitAt n l

setElem 1 (h:t) x = (x:t)
setElem i (h:t) x = h:(setElem (i-1) t x)

clearDiagonal i [] = []
clearDiagonal  i (h:t) = (setElem i h 0):(clearDiagonal (i+1) t)

railsNetwork :: Int -> Int -> [[Int]]
railsNetwork n seed = clearDiagonal 1 (getMatrix n (getRandomNumbers (n*n) seed)) 

--edge
pullLever :: Int -> Int -> Int-> [[Int]] -> [[Int]]
pullLever seed s d m |s==d = m
					 |otherwise = setElem s m x where x=setElem d (m!!(s-1)) ((getRandomNumbers 1 seed)!!0)

--PQ
createNode x s | x==s = Node x 0
			   | otherwise = Node x 9999

initializeSource :: Int -> [[Int]] -> PriorityQueue
initializeSource s m = enqueueAll [createNode x s | x<-[1..(length m)]] Empty

--shortest path
getWeight m s d = (m!!(s-1))!!(d-1)

getSmallerKey kCurrent kHead w = if (w>0 && (kCurrent+w)<kHead) then (kCurrent + w) else kHead

updateQueue m current Empty result = result
updateQueue m (Node nCurrent kCurrent) (PQ (Node nHead kHead) q) result = updateQueue m (Node nCurrent kCurrent) q (enqueue (Node nHead (getSmallerKey kCurrent kHead w)) result) where w = getWeight m nCurrent nHead
																										 

computeShortestPathCost :: Int -> [[Int]] -> PriorityQueue -> Int
computeShortestPathCost d m Empty = error("You are searching for a destination station that is not in the network")
computeShortestPathCost d m (PQ (Node n k) q) = if(n==d) then k 

														 else (computeShortestPathCost d m (updateQueue m (Node n k) q Empty))

-- generate x random numbers, using a seeded generator, in the closed range [0,9].
getRandomNumbers :: Int -> Int -> [Int]
getRandomNumbers x seed = 
	let g = mkStdGen seed
	in take x (randomRs (0, 9) g)

-- format the 2D list 
printRailsNetwork :: [[Int]] -> IO ()
printRailsNetwork network = mapM_ print network



-----------------------------------------------------------------------TESTS--------------------------------------------------------------------------------

-----------------Begin railsNetwork Tests--------------------------

test1 = TestCase(assertEqual "public1" [[0,1,0,9,5],[3,0,7,9,8],[9,6,0,0,7],[6,7,1,0,6],[9,5,3,2,0]] (railsNetwork 5 12345))

test2 = TestCase(assertEqual "public2" [[0,9,6,9,0,4,7,8,8,8],[1,0,6,9,5,4,9,7,9,9],[3,9,0,3,8,4,8,5,0,6],[1,9,0,0,0,3,3,3,8,7],[5,2,8,2,0,0,2,4,2,5],[1,6,0,6,9,0,0,5,2,6],[7,4,0,5,8,9,0,9,4,7],[5,9,6,9,4,2,4,0,8,2],[5,4,8,7,1,8,0,5,0,2],[2,1,1,3,7,4,8,1,6,0]] (railsNetwork 10 9384573))

test3 = TestCase(assertEqual "public3" [[0,0,6,0,4,0,3,9,6,7,8,3,7,5,3],[4,0,2,2,4,6,3,9,5,7,6,0,8,6,6],[8,4,0,1,7,7,8,8,2,3,0,2,7,2,6],[7,1,6,0,1,1,2,2,6,6,8,9,3,0,2],[3,5,4,9,0,5,2,1,0,9,3,2,5,9,6],[9,1,5,4,1,0,1,9,8,4,1,9,5,3,8],[4,8,3,0,7,3,0,1,5,9,7,7,6,3,8],[4,4,7,6,1,0,0,0,1,5,3,9,8,8,5],[3,6,1,6,2,3,8,2,0,3,2,1,9,8,1],[0,5,4,9,9,1,7,2,1,0,3,4,3,4,0],[0,2,8,5,3,1,6,4,7,1,0,4,6,6,5],[9,9,9,0,6,0,6,5,0,8,6,0,3,9,4],[5,1,9,3,9,6,7,3,7,6,0,8,0,8,5],[9,7,0,1,2,2,4,6,5,1,0,7,9,0,9],[7,6,3,1,7,6,8,9,5,2,8,5,4,3,0]] (railsNetwork 15 347))

test4 = TestCase(assertEqual "public4" [[0,1,4,7,4,0,6,1,2,3,4,1,4,1,4,0,7,4,7,6],[5,0,5,6,9,8,6,4,2,4,3,8,4,0,4,6,6,4,5,7],[5,1,0,1,8,4,0,5,8,3,3,0,2,2,8,9,8,1,1,0],[5,5,2,0,3,6,8,4,2,3,4,3,9,6,5,1,6,8,8,8],[3,5,0,6,0,4,4,8,3,2,6,8,3,9,1,2,1,3,7,3],[6,2,3,9,4,0,1,9,9,0,8,6,8,6,6,1,5,0,3,0],[1,4,8,2,3,3,0,0,0,8,5,2,7,1,2,6,3,0,2,5],[1,6,7,7,2,1,5,0,0,6,5,0,9,6,7,9,4,6,3,0],[1,2,8,8,3,8,4,2,0,6,7,2,6,9,8,8,3,4,8,3],[2,9,7,2,9,9,6,5,8,0,4,6,4,6,9,0,6,9,1,9],[2,0,0,6,0,2,7,6,6,1,0,9,0,7,6,4,6,6,2,1],[2,6,9,9,2,6,7,9,6,2,3,0,3,1,2,9,0,2,6,9],[4,7,1,3,9,1,6,5,1,3,4,3,0,4,2,0,5,1,6,2],[8,3,7,7,8,7,4,2,4,0,0,9,6,0,8,9,2,8,3,4],[2,5,6,2,5,9,8,3,2,8,3,6,5,2,0,4,1,2,9,9],[3,4,5,5,6,0,6,9,4,5,0,6,6,8,5,0,1,0,1,1],[5,3,6,8,1,5,9,1,7,5,5,5,0,3,4,7,0,9,4,3],[5,0,8,4,6,7,8,8,5,8,8,3,1,9,4,7,0,0,8,5],[7,0,7,3,4,9,1,8,0,6,1,7,8,1,5,0,1,1,0,5],[6,9,4,4,9,8,4,2,0,0,8,6,3,7,4,7,0,2,5,0]]
 (railsNetwork 20 4588544))

test5 = TestCase(assertEqual "public5" [[0,6,8,3,5,9,5,7,6,7,9,6,4,3,8,7,0,5,4,4,5,7,0,5,8],[5,0,1,1,9,9,6,1,6,7,6,3,7,3,3,8,7,9,2,5,5,1,9,1,8],[0,3,0,4,5,4,3,6,4,3,1,1,3,5,1,9,7,0,5,7,7,1,4,9,2],[9,5,1,0,9,6,1,0,2,9,1,8,3,8,3,2,6,0,8,6,9,8,9,9,6],[6,5,8,1,0,4,2,3,7,5,4,0,7,8,3,6,1,0,6,3,5,3,9,3,5],[5,5,5,1,1,0,4,7,3,3,3,6,6,0,5,7,4,2,7,7,5,5,2,6,6],[0,9,1,2,7,3,0,4,0,6,5,5,2,6,8,4,3,8,1,0,4,3,0,7,7],[6,2,0,7,5,9,7,0,3,8,9,5,6,1,4,4,2,2,6,0,4,2,0,2,2],[2,3,9,1,6,2,4,2,0,2,2,3,0,8,6,2,3,9,0,1,4,7,7,5,9],[1,6,9,8,0,5,6,8,5,0,3,5,0,9,1,6,2,2,9,5,9,7,5,8,1],[8,4,7,3,7,4,2,3,0,9,0,4,8,3,2,3,3,8,5,4,7,2,1,8,0],[4,2,5,1,8,0,3,3,6,9,3,0,9,8,9,0,0,3,6,9,6,6,9,6,0],[9,0,3,7,1,6,3,6,4,5,9,2,0,8,2,8,0,5,9,9,1,0,3,6,8],[4,3,2,3,4,3,2,9,6,2,4,5,1,0,7,7,2,6,3,1,5,3,0,0,0],[6,0,5,3,2,1,0,1,0,6,3,7,7,7,0,6,1,6,8,7,1,0,0,1,9],[4,7,5,1,7,2,1,8,0,5,6,5,4,1,7,0,9,0,2,5,5,7,9,8,2],[6,2,0,1,9,4,2,2,8,6,1,4,1,5,4,0,0,4,4,7,5,8,4,1,0],[2,7,2,1,6,7,2,1,2,0,3,4,6,5,6,1,9,0,1,5,2,9,4,9,8],[0,9,0,3,6,5,9,2,7,9,0,6,6,1,9,5,8,9,0,7,7,8,1,5,8],[6,5,5,5,9,9,4,4,1,2,7,9,7,2,3,2,9,6,7,0,6,1,2,6,6],[3,5,8,4,1,8,7,7,6,8,8,3,7,5,8,8,7,6,3,4,0,5,4,8,7],[5,5,3,6,4,2,8,0,0,9,2,9,0,0,6,5,4,6,3,4,5,0,2,9,8],[7,8,7,7,4,1,4,8,1,0,3,6,3,3,9,2,2,8,1,3,2,6,0,8,3],[8,6,7,2,3,6,2,8,6,4,7,9,8,2,3,2,6,4,3,2,8,1,8,0,3],[5,6,5,4,1,4,1,2,6,8,0,7,7,1,4,6,2,5,3,0,8,0,6,5,0]] (railsNetwork 25 92741563))

-----------------End railsNetwork Tests--------------------------


-----------------Begin pullLever Tests---------------------------

test6 = TestCase(assertEqual "public6" [[0,1,0,9,5],[3,0,7,9,8],[9,6,0,0,7],[6,7,1,0,0],[9,5,3,2,0]] (pullLever 98765 4 5 (railsNetwork 5 12345)))

test7 = TestCase(assertEqual "public7" [[0,9,6,9,0,4,9,8,8,8],[1,0,6,9,5,4,9,7,9,9],[3,9,0,3,8,4,8,5,0,6],[1,9,0,0,0,3,3,3,8,7],[5,2,8,2,0,0,2,4,2,5],[1,6,0,6,9,0,0,5,2,6],[7,4,0,5,8,9,0,9,4,7],[5,9,6,9,4,2,4,0,8,2],[5,4,8,7,1,8,0,5,0,2],[2,1,1,3,7,4,8,1,6,0]] (pullLever 8264 1 7 (railsNetwork 10 9384573)))

test8 = TestCase(assertEqual "public8" [[0,0,6,0,4,0,3,9,6,7,8,3,7,5,3],[4,0,2,2,4,6,3,9,5,7,6,0,8,6,6],[8,4,0,1,7,7,8,8,2,3,0,2,7,2,6],[7,1,6,0,1,1,2,2,6,6,8,9,3,0,2],[3,5,4,9,0,5,2,1,0,9,3,2,5,9,6],[9,1,5,4,1,0,1,9,8,4,1,9,5,3,8],[4,8,3,0,7,3,0,1,5,9,7,7,6,3,8],[4,4,7,6,1,0,0,0,1,5,3,9,8,8,5],[3,6,1,6,2,3,8,2,0,3,2,1,9,8,1],[0,5,4,9,9,1,7,2,1,0,3,4,3,4,0],[0,2,8,5,3,1,6,4,7,1,0,4,9,6,5],[9,9,9,0,6,0,6,5,0,8,6,0,3,9,4],[5,1,9,3,9,6,7,3,7,6,0,8,0,8,5],[9,7,0,1,2,2,4,6,5,1,0,7,9,0,9],[7,6,3,1,7,6,8,9,5,2,8,5,4,3,0]] (pullLever 6353875 11 13 (railsNetwork 15 347)))

test9 = TestCase(assertEqual "public9" [[0,1,4,7,4,0,6,1,2,3,4,1,4,1,4,0,7,4,7,6],[5,0,5,6,9,8,6,4,2,4,3,8,4,0,4,6,6,4,5,7],[5,1,0,1,8,4,0,5,8,3,3,0,2,2,8,9,8,1,1,0],[5,5,2,0,3,6,8,4,2,3,4,3,9,6,5,1,6,8,8,8],[3,5,0,6,0,4,4,8,3,2,6,8,3,9,1,2,1,3,7,3],[6,2,3,9,4,0,1,9,9,0,8,6,8,6,6,1,5,7,3,0],[1,4,8,2,3,3,0,0,0,8,5,2,7,1,2,6,3,0,2,5],[1,6,7,7,2,1,5,0,0,6,5,0,9,6,7,9,4,6,3,0],[1,2,8,8,3,8,4,2,0,6,7,2,6,9,8,8,3,4,8,3],[2,9,7,2,9,9,6,5,8,0,4,6,4,6,9,0,6,9,1,9],[2,0,0,6,0,2,7,6,6,1,0,9,0,7,6,4,6,6,2,1],[2,6,9,9,2,6,7,9,6,2,3,0,3,1,2,9,0,2,6,9],[4,7,1,3,9,1,6,5,1,3,4,3,0,4,2,0,5,1,6,2],[8,3,7,7,8,7,4,2,4,0,0,9,6,0,8,9,2,8,3,4],[2,5,6,2,5,9,8,3,2,8,3,6,5,2,0,4,1,2,9,9],[3,4,5,5,6,0,6,9,4,5,0,6,6,8,5,0,1,0,1,1],[5,3,6,8,1,5,9,1,7,5,5,5,0,3,4,7,0,9,4,3],[5,0,8,4,6,7,8,8,5,8,8,3,1,9,4,7,0,0,8,5],[7,0,7,3,4,9,1,8,0,6,1,7,8,1,5,0,1,1,0,5],[6,9,4,4,9,8,4,2,0,0,8,6,3,7,4,7,0,2,5,0]] (pullLever 8346 6 18 (railsNetwork 20 4588544)))

test10 = TestCase(assertEqual "public10" [[0,6,8,3,5,9,5,7,6,7,9,6,4,3,8,7,0,5,4,4,5,7,0,5,8],[5,0,1,1,9,9,6,1,6,7,6,3,7,3,3,8,7,9,2,5,5,1,9,1,8],[0,3,0,4,5,4,3,6,6,3,1,1,3,5,1,9,7,0,5,7,7,1,4,9,2],[9,5,1,0,9,6,1,0,2,9,1,8,3,8,3,2,6,0,8,6,9,8,9,9,6],[6,5,8,1,0,4,2,3,7,5,4,0,7,8,3,6,1,0,6,3,5,3,9,3,5],[5,5,5,1,1,0,4,7,3,3,3,6,6,0,5,7,4,2,7,7,5,5,2,6,6],[0,9,1,2,7,3,0,4,0,6,5,5,2,6,8,4,3,8,1,0,4,3,0,7,7],[6,2,0,7,5,9,7,0,3,8,9,5,6,1,4,4,2,2,6,0,4,2,0,2,2],[2,3,9,1,6,2,4,2,0,2,2,3,0,8,6,2,3,9,0,1,4,7,7,5,9],[1,6,9,8,0,5,6,8,5,0,3,5,0,9,1,6,2,2,9,5,9,7,5,8,1],[8,4,7,3,7,4,2,3,0,9,0,4,8,3,2,3,3,8,5,4,7,2,1,8,0],[4,2,5,1,8,0,3,3,6,9,3,0,9,8,9,0,0,3,6,9,6,6,9,6,0],[9,0,3,7,1,6,3,6,4,5,9,2,0,8,2,8,0,5,9,9,1,0,3,6,8],[4,3,2,3,4,3,2,9,6,2,4,5,1,0,7,7,2,6,3,1,5,3,0,0,0],[6,0,5,3,2,1,0,1,0,6,3,7,7,7,0,6,1,6,8,7,1,0,0,1,9],[4,7,5,1,7,2,1,8,0,5,6,5,4,1,7,0,9,0,2,5,5,7,9,8,2],[6,2,0,1,9,4,2,2,8,6,1,4,1,5,4,0,0,4,4,7,5,8,4,1,0],[2,7,2,1,6,7,2,1,2,0,3,4,6,5,6,1,9,0,1,5,2,9,4,9,8],[0,9,0,3,6,5,9,2,7,9,0,6,6,1,9,5,8,9,0,7,7,8,1,5,8],[6,5,5,5,9,9,4,4,1,2,7,9,7,2,3,2,9,6,7,0,6,1,2,6,6],[3,5,8,4,1,8,7,7,6,8,8,3,7,5,8,8,7,6,3,4,0,5,4,8,7],[5,5,3,6,4,2,8,0,0,9,2,9,0,0,6,5,4,6,3,4,5,0,2,9,8],[7,8,7,7,4,1,4,8,1,0,3,6,3,3,9,2,2,8,1,3,2,6,0,8,3],[8,6,7,2,3,6,2,8,6,4,7,9,8,2,3,2,6,4,3,2,8,1,8,0,3],[5,6,5,4,1,4,1,2,6,8,0,7,7,1,4,6,2,5,3,0,8,0,6,5,0]] (pullLever 7278957 3 9 (railsNetwork 25 92741563)))

-----------------End pullLever Tests--------------------------


-----------------Begin initializeSource Tests-----------------

test11 = TestCase(assertEqual "public11" (PQ (Node 4 0) (PQ (Node 1 9999) (PQ (Node 2 9999) (PQ (Node 3 9999) (PQ (Node 5 9999) Empty))))) (initializeSource 4 (railsNetwork 5 12345)))

test12 = TestCase(assertEqual "public12" (PQ (Node 1 0) (PQ (Node 2 9999) (PQ (Node 3 9999) (PQ (Node 4 9999) (PQ (Node 5 9999) (PQ (Node 6 9999) (PQ (Node 7 9999) (PQ (Node 8 9999) (PQ (Node 9 9999) (PQ (Node 10 9999) Empty)))))))))) (initializeSource 1 (railsNetwork 10 9384573)))

test13 = TestCase(assertEqual "public13" (PQ (Node 6 0) (PQ (Node 1 9999) (PQ (Node 2 9999) (PQ (Node 3 9999) (PQ (Node 4 9999) (PQ (Node 5 9999) (PQ (Node 7 9999) (PQ (Node 8 9999) (PQ (Node 9 9999) (PQ (Node 10 9999) (PQ (Node 11 9999) (PQ (Node 12 9999) (PQ (Node 13 9999) (PQ (Node 14 9999) (PQ (Node 15 9999) Empty))))))))))))))) (initializeSource 6 (railsNetwork 15 347)))

test14 = TestCase(assertEqual "public14" (PQ (Node 15 0) (PQ (Node 1 9999) (PQ (Node 2 9999) (PQ (Node 3 9999) (PQ (Node 4 9999) (PQ (Node 5 9999) (PQ (Node 6 9999) (PQ (Node 7 9999) (PQ (Node 8 9999) (PQ (Node 9 9999) (PQ (Node 10 9999) (PQ (Node 11 9999) (PQ (Node 12 9999) (PQ (Node 13 9999) (PQ (Node 14 9999) (PQ (Node 16 9999) (PQ (Node 17 9999) (PQ (Node 18 9999) (PQ (Node 19 9999) (PQ (Node 20 9999) Empty)))))))))))))))))))) (initializeSource 15 (railsNetwork 20 4588544)))

test15 = TestCase(assertEqual "public15" (PQ (Node 25 0) (PQ (Node 1 9999) (PQ (Node 2 9999) (PQ (Node 3 9999) (PQ (Node 4 9999) (PQ (Node 5 9999) (PQ (Node 6 9999) (PQ (Node 7 9999) (PQ (Node 8 9999) (PQ (Node 9 9999) (PQ (Node 10 9999) (PQ (Node 11 9999) (PQ (Node 12 9999) (PQ (Node 13 9999) (PQ (Node 14 9999) (PQ (Node 15 9999) (PQ (Node 16 9999) (PQ (Node 17 9999) (PQ (Node 18 9999) (PQ (Node 19 9999) (PQ (Node 20 9999) (PQ (Node 21 9999) (PQ (Node 22 9999) (PQ (Node 23 9999) (PQ (Node 24 9999) Empty))))))))))))))))))))))))) (initializeSource 25 (railsNetwork 25 92741563)))

-----------------End initializeSource Tests-------------------

-------------Begin computeShortestPathCost Tests--------------

test16 = TestCase(assertEqual "public16" 0 (computeShortestPathCost 4 (railsNetwork 5 12345) (initializeSource 4 (railsNetwork 5 12345))))

test17 = TestCase(assertEqual "public17" 6 (computeShortestPathCost 9 (railsNetwork 10 9384573) (initializeSource 1 (railsNetwork 10 9384573))))

test18 = TestCase(assertEqual "public18" 4 (computeShortestPathCost 15 (railsNetwork 15 347) (initializeSource 6 (railsNetwork 15 347))))

test19 = TestCase(assertEqual "public19" 3 (computeShortestPathCost 2 (railsNetwork 20 4588544) (initializeSource 15 (railsNetwork 20 4588544))))

test20 = TestCase(assertEqual "public20" 2 (computeShortestPathCost 20 (railsNetwork 25 92741563) (initializeSource 25 (railsNetwork 25 92741563))))

--------------End computeShortestPathCost Tests---------------

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8, TestLabel "test9" test9, TestLabel "test10" test10, TestLabel "test11" test11, TestLabel "test12" test12, TestLabel "test13" test13, TestLabel "test14" test14, TestLabel "test15" test15, TestLabel "test16" test16, TestLabel "test17" test17, TestLabel "test18" test18, TestLabel "test19" test19, TestLabel "test20" test20]




