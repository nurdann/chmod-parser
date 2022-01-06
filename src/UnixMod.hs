module UnixMod where

import Data.Bits ((.|.), (.&.), bit, testBit)
import Debug.Trace (trace)
{-
700 -> u+rwx,g-rwx,o-rwx
600 -> u+rw-x,g-rwx,o-rwx
611 -> u+rw-x,g-rw+x,o-rw+x

owner/user, group, other take positional value in range [0, 7]

4000 (setuid) -> u+rwsx or u+rws (setuid bit ignored without +x)
2000 (setgid) -> g+rwsx or g+rws (getgid bit ignored without +x)
1000 (sticky bit) -> g+t (directories only; all files under are modifiable my owners only)
-}

toSign :: Int -> Char
toSign 0 = '-'
toSign 1 = '+'
toSign x = error ("Expected 0 or 1, but got " ++ show x)

octToRwx :: Int -> String
octToRwx x = go x "xwr" []
  where
    go _ [] acc = acc
    go x (p:ps) acc = go (x `div` 2) ps (toSign (x `mod` 2) : p : acc)

{-
srwx -> sst rwx rwx rwx
4000 -> 100 000 000 000
4246 -> 100 010 100 110
-}
data Rwx = Rwx
 { canRead :: Bool
 , canWrite :: Bool
 , canExec :: Bool
 }
 deriving Show

instance Eq Rwx where
  Rwx {canRead=r1, canWrite=w1, canExec=e1} == Rwx {canRead=r2, canWrite=w2, canExec=e2} =
    r1 == r2 && w1 == w2 && e1 == e2

rwxToInt :: Rwx -> Int
rwxToInt (Rwx {canRead=canRead, canWrite=canWrite, canExec=canExec}) =
  bit 2 * (fromEnum canRead) + bit 1 * (fromEnum canWrite) + fromEnum canExec

intToRwx :: Int -> Rwx
intToRwx x | x > 7 || x < 0 = error $ "Expected a number in range [0-7], but got " ++ show x
intToRwx x =
  Rwx { canRead = nth 2
      , canWrite = nth 1
      , canExec = nth 0
      }
  where nth = testBit x

instance Enum Rwx where
  fromEnum = rwxToInt
  toEnum = intToRwx

-- instance Show Rwx where
--   show = show . fromEnum

data Permission = Permission
  { setuid :: Bool
  , setgid :: Bool
  , sticky :: Bool
  , owner :: Rwx
  , group :: Rwx
  , other :: Rwx
  }
  deriving Show

absToSym :: Int -> Permission
absToSym x | x < 0 || x > 7777 = error $ "Expected a permission in range [0, 7777], but got" ++ show x
absToSym x = Permission
  { setuid = testBit (nthDigit x 3) 2
  , setgid = testBit (nthDigit x 3) 1
  , sticky = testBit (nthDigit x 3) 0
  , owner = intToRwx (nthDigit x 2)
  , group = intToRwx (nthDigit x 1)
  , other = intToRwx (nthDigit x 0)
  }

nthDigit :: Int -> Int -> Int
nthDigit x n
  | nth > 7 = error $ "Expected a digit in range [0-7], but got " ++ show nth
  | otherwise = nth
  where nth = x `div` 10^n `mod` 10

noRwx = Rwx {canRead = False, canWrite = False, canExec = False}
noPermission = Permission {setuid = False, setgid = False, sticky = False, owner = noRwx, group = noRwx, other = noRwx}
noGroupOther (Permission {group = group, other = other}) = group == noRwx && other == noRwx        
