module Type where

import Linear.Affine
import Linear.V2

type Pos = Point V2 Double

type Size = V2 Double

--

data Segment = Seg Pos Pos

data Triangle = Tri Pos Pos Pos

type Polygon = [Pos]

data Rectangle = Rect Pos Size

--

data FieldOfView = Fov Pos [Triangle]
