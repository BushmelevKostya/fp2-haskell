module Main (main) where

import Test.HUnit
import Lib
import GHC.Arr (Array, array, (!), (//), elems)
import Data.Hashable (Hashable, hash)

testCreateSet :: Test
testCreateSet = TestCase $ do
    let set = createSet :: SCSet String
    assertEqual "Initial currentSize should be 0" 0 (currentSize set)
    assertEqual "Initial maxSize should be 8" 8 (maxSize set)
    assertEqual "Initial loadFactor should be 0.75" 0.75 (loadFactor set)
    assertEqual "Initial arrayData should be empty lists" (replicate 8 []) (elems (arrayData set))

testAdd :: Test
testAdd = TestCase $ do
    let set = add "test" createSet
    assertEqual "Current size should increase after add" 1 (currentSize set)
    assertEqual "Added element should be present" ["test"] ((arrayData set) ! (hash "test" `mod` 8))

testRemove :: Test
testRemove = TestCase $ do
    let set = add "test" createSet
    let setAfterRemove = remove "test" set
    assertEqual "Current size should decrease after remove" 0 (currentSize setAfterRemove)
    assertEqual "Removed element should not be present" [] ((arrayData setAfterRemove) ! (hash "test" `mod` 8))

testAddAll :: Test
testAddAll = TestCase $ do
    let set = addAll ["test1", "test2", "test3"] createSet
    assertEqual "All elements should be added" 3 (currentSize set)
    assertBool "test1 should be present" ("test1" `elem` getDataAsList set)
    assertBool "test2 should be present" ("test2" `elem` getDataAsList set)
    assertBool "test3 should be present" ("test3" `elem` getDataAsList set)

testMapValues :: Test
testMapValues = TestCase $ do
    let set = addAll ["apple", "banana"] createSet
    let mappedSet = mapValues (++ " fruit") set
    assertBool "apple fruit should be in mapped set" ("apple fruit" `elem` getDataAsList mappedSet)
    assertBool "banana fruit should be in mapped set" ("banana fruit" `elem` getDataAsList mappedSet)

testFilterValues :: Test
testFilterValues = TestCase $ do
    let set = addAll ["apple", "banana", "cherry"] createSet
    let filteredSet = filterValues (\v -> length v > 5) set
    assertEqual "Filtered set should only contain 'banana' and 'cherry'" ["banana", "cherry"] (getDataAsList filteredSet)

testFoldable :: Test
testFoldable = TestCase $ do
    let set = addAll [1, 2, 3] createSet :: SCSet Int
    assertEqual "Sum of elements in set should be 6" 6 (foldr (+) 0 set)

testSemigroup :: Test
testSemigroup = TestCase $ do
    let set1 = addAll [1, 2] createSet :: SCSet Int
    let set2 = addAll [3, 4] createSet :: SCSet Int
    let combinedSet = set1 <> set2
    assertEqual "Combined set should contain elements from both sets" [1, 2, 3, 4] (getDataAsList combinedSet)

testMonoid :: Test
testMonoid = TestCase $ do
    let set = addAll [1, 2, 3] createSet :: SCSet Int
    assertEqual "Mempty combined with a set should equal the set" (mempty <> set) set
    assertEqual "Set combined with mempty should equal the set" (set <> mempty) set

main :: IO Counts
main = runTestTT $ TestList
    [ testCreateSet
    , testAdd
    , testRemove
    , testAddAll
    , testMapValues
    , testFilterValues
    , testFoldable
    , testSemigroup
    , testMonoid
    ]
