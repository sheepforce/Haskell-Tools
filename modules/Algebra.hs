module Algebra
( Vector(..)
, radian2deg
, deg2radian
, dihedralPoint
, angleVec
, normalVec
, paramForm
, crossProd
, scalarProd
, lengthVec
, distPoint
, anglePoint
, addVec
, scalMultVec
, subVec
, list2vec
, vec2list
, cart2spherical
, spherical2cart
, rotatePoint
, rotateGeom
--
, recenterGeom
) where

{- defines a vector in R³ -}
data Vector a = Vector a a a deriving (Show,Ord,Eq)


{- standard algebraic functions -}

{- converts radian to degree (360 is a circle) -}
radian2deg :: (Floating a) => a -> a
radian2deg a = a / ((2 * pi) / 360.0)

{- converts degree to radian (360 is a circle) -}
deg2radian :: (Floating a) => a -> a
deg2radian a = a / (360.0 / (2 * pi))

dihedralPoint :: (Floating a) => Vector a -> Vector a -> Vector a -> Vector a -> a
dihedralPoint a b c d = angleVec nVecABCPlane nVecBCDPlane
  where nVecABCPlane = crossProd (subVec a b) (subVec c b)
	nVecBCDPlane = crossProd (subVec b c) (subVec d c)

{- THIS FUNCTION IS NOT WORKING CORRECTLY
{- dihedralPoint takes 4 Points that define two planes and calculates the dihedral angle between them -}
dihedralPoint :: (Floating a) => Vector a -> Vector a -> Vector a -> Vector a -> a
dihedralPoint a b c d = angleVec nVecABCPlane nVecBCDPlane
  where nVecABCPlane = normalVec abcPlane
	nVecBCDPlane = normalVec bcdPlane
	abcPlane     = paramForm b a c
	bcdPlane     = paramForm c b d
-}

{- angleVec takes two vectors and calculates the angle between them -}
angleVec :: (Floating a) => Vector a -> Vector a -> a
angleVec a b = acos clambda
  where clambda  = upfrac / downfrac
	upfrac   = scalarProd a b
	downfrac = (lengthVec a) * (lengthVec b)

{- normalVec takes a plane defined in parameter form and returns the normal vector -}
normalVec :: (Num a) => [Vector a] -> Vector a
normalVec a = crossProd b c
  where b = a!!1
	c = a!!2

{- paramForm takes 3 points a, b, c in R³ and builds a plane from them,
defined in the parameter Form -}
paramForm :: (Num a) => Vector a -> Vector a -> Vector a -> [Vector a]
paramForm (Vector a b c) (Vector d e f) (Vector g h i) = [plainElem, rElem, sElem]
  where plainElem = Vector a b c
	rElem     = Vector (d - a) (b - e) (f - c)
	sElem     = Vector (g - a) (h - b) (i - c)

{- calculates the cross product of two vectors -}
crossProd :: (Num a) => Vector a -> Vector a -> Vector a
crossProd (Vector a b c) (Vector d e f) = Vector (b * f - c * e) (c * d - a * f) (a * e - b * d)

{- scalarProd calculates the scalar product of two vectors in R³ -}
scalarProd :: (Num a) => Vector a -> Vector a -> a
scalarProd (Vector a b c) (Vector d e f) = a * d + b * e + c * f

{- lengthVec takes a vector and calculates its length aka absolute value -}
lengthVec :: (Floating a) => Vector a -> a
lengthVec (Vector a b c) = (a^2 + b^2 + c^2)**(0.5)

{- distPoint takes two points (defined as vectors) and returns their distance -}
distPoint :: (Floating a) => Vector a -> Vector a -> a
distPoint a b = lengthVec (subVec a b)

{- anglePoint takes three points (defined as vectors and returns the angle between 1 2 3 -}
anglePoint :: (Floating a) => Vector a -> Vector a -> Vector a -> a
anglePoint a b c = angleVec (subVec a b) (subVec c b)

{- addVec takes two vectors and adds them elementwise -}
addVec :: (Num a) => Vector a -> Vector a -> Vector a
addVec (Vector a b c) (Vector d e f) = Vector (a + d) (b + e) (c + f)

{- scalMultVec takes a scalar and a vector and multiplies the vector with the scalar -}
scalMultVec :: (Num a) => a -> Vector a -> Vector a
scalMultVec a (Vector b c d) = Vector (a * b) (a * c) (a * d)

{- subVec takes two vectors and substracts them elementwise -}
subVec :: (Num a) => Vector a -> Vector a -> Vector a
subVec (Vector a b c) (Vector d e f) = Vector (a - d) (b - e) (c - f)

{- list2vec takes a list of 3 numeric values an converts it to a vector -}
list2vec :: (Num a) => [a] -> Vector a
list2vec [a,b,c] = Vector a b c

{- vec2list takes a vector and converts it to a list of 3 values -}
vec2list :: (Num a) => Vector a -> [a]
vec2list (Vector a b c) = [a,b,c]

{- cart2spherical takes a vector in cartesian coordinates and converts it
to a vector in spherical coordinates with the ordering r theta phi based on the
Wikipedia notation https://en.wikipedia.org/wiki/Spherical_coordinate_system -}
cart2spherical :: (RealFloat a, Ord a) => Vector a -> Vector a
cart2spherical (Vector x y z) = Vector r theta phi 
  where
    r     = lengthVec (Vector x y z)
    theta = acos (z / r)
    phi   = atan2 y x

{- spherical2cart takes a vector in spherical coordinates and converts it
to a vector in cartesian coordinates with the ordering x y z based on the
Wikipedia notation https://en.wikipedia.org/wiki/Spherical_coordinate_system -}
spherical2cart :: (Floating a) => Vector a -> Vector a
spherical2cart (Vector r theta phi) = Vector x y z
  where x = r * sin theta * cos phi
	y = r * sin theta * sin phi
	z = r * cos theta


{- ###################################################### -}
{- editing functions, mainly useful for chemicals systems -}
{- ###################################################### -}

{- recenterGeom takes a list of atom coordinates (molecule) a and substracts a vector b
of each. If b is an atomic position itself, it will be centered -}
recenterGeom :: (Floating a) => [Vector a] -> Vector a -> [Vector a]
recenterGeom a b = [subVec c b | c <- a]

{- rotatePoint takes a point in spherical coordinates an rotates in 2 axis around the origin
about rotTheta and rotPhi -}
rotatePoint :: (RealFloat a) => Vector a -> a -> a -> Vector a
rotatePoint (Vector r theta phi) rotTheta rotPhi = Vector r (theta + rotTheta) (phi + rotPhi)

{- rotates a list of coordinates (atoms) about rotTheta and rotPhi -}
rotateGeom :: (RealFloat a) => [Vector a] -> a -> a -> [Vector a]
rotateGeom a rotTheta rotPhi = [addVec b c | b <- a]
  where c = Vector 0.0 rotTheta rotPhi


{- ############################## -}
{- helper functions, not exported -}
{- ############################## -}