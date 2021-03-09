-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Data structures can be recursive! That is, they can have one or
  more of their constructors that refer back to the original type.
  As with recursive functions, there must be a *base case* with
  no recursive reference, as it would be impossible to construct
  otherwise.

- The 'List' type is defined in this way. Here's a way we could define a
  similar looking type:

data List a =
  EmptyList       | -- < Base Case Constructor
  Cons a (List a) | -- < Recursive Constructor

- In the actual list type, 'EmptyList' is '[]', and 'Cons' is the '(:)' operator.
  Here are some examples of constructor:

l1 :: List Int
l1 = EmptyList

l2 :: List Int
l2 = Cons 4 EmptyList

l3 :: List Int
l3 = Cons 5 (Cons 4 EmptyList)

l4 :: List Int
l4 = Cons 5 l3

- These are similar to these real lists:

l1 = []
l2 = 4 : []     -- Or [4]
l3 = 5 : 4 : [] -- Or [5, 4]
l4 = 5 : l3

-}

-- TODO:
-- Define a Binary Tree type containing 'Int' values.
-- It should have two constructors.
-- The first should be 'EmptyNode'.
-- The second should be called 'ValueNode'. It should contain an 'Int' which
-- is the value at the node, and then it should have a 'left' and a 'right' subtree,
-- which will be recursive BinaryTree elements.
data BinaryTree = BinaryTree

-- Take the sum of all the values in a BinaryTree!
-- You'll need to make multiple recursive calls at once!
sumTree :: BinaryTree -> Int
sumTree = ???

-- Determine if your tree is a valid Binary Search Tree (BST)
-- For every ValueNode in the tree:
--   1. All elements in its left subtree are smaller than the value at the node.
--   2. All elements in its right subtree are larger than the value at the node.
-- You might want a helper that takes two 'bounding' elements in addition to a subtree.
isValidBst :: BinaryTree -> Bool
isValidBst = ???

tree1 :: BinaryTree
tree1 = ValueNode 8
  (ValueNode 3 (ValueNode 1 EmptyNode EmptyNode) (ValueNode 6 (ValueNode 4 EmptyNode EmptyNode) (ValueNode 7 EmptyNode EmptyNode)))
  (ValueNode 10 EmptyNode (ValueNode 14 (ValueNode 13 EmptyNode EmptyNode) EmptyNode))

tree2 :: BinaryTree
tree2 = ValueNode 17
  (ValueNode 3 (ValueNode 1 EmptyNode EmptyNode) (ValueNode 6 (ValueNode 4 EmptyNode EmptyNode) (ValueNode 24 EmptyNode EmptyNode)))
  (ValueNode 10 EmptyNode (ValueNode 14 (ValueNode 13 EmptyNode EmptyNode) EmptyNode))

main :: IO ()
main = defaultMain $ testGroup "Recusion4" $
  [ testCase "sumTree 1" $ sumTree EmptyNode @?= 0
  , testCase "sumTree 2" $ sumTree (ValueNode 4 EmptyNode EmptyNode) @?= 4
  , testCase "sumTree 3" $ sumTree (ValueNode 4 (ValueNode 5 EmptyNode EmptyNode) EmptyNode) @?= 9
  , testCase "sumTree 4" $ sumTree tree1 @?= 66
  , testCase "sumTree 5" $ sumTree tree2 @?= 92
  , testCase "isValidBst 1" $ isValidBst EmptyNode @?= True
  , testCase "isValidBst 2" $ isValidBst (ValueNode 4 EmptyNode EmptyNode) @?= True
  , testCase "isValidBst 3" $ isValidBst (ValueNode 4 (ValueNode 5 EmptyNode EmptyNode) EmptyNode) @?= False
  , testCase "isValidBst 4" $ isValidBst tree1 @?= True
  , testCase "isValidBst 5" $ isValidBst tree2 @?= False
  ]
