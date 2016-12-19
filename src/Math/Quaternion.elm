module Math.Quaternion exposing (..)

{-| A quaternion module based on `Math.Vector4`. Geared towards 3D graphics
and use with `Graphics.WebGL`. All quaternions are immutable.

# Create
@docs Quat, quat, identity, fromAxisAngle

# Get and Set
The set functions create a new copy of the quaternion, updating a single field.

@docs getX, getY, getZ, getW, setX, setY, setZ, setW

# Operations
@docs add, sub, mul, conjugate, negate, scale, dot, normalize,
      length, lengthSquared

# Conversions
@docs toTuple, fromTuple, toRecord, fromRecord, toMatrix
-}

import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as M4
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Vector4 exposing (Vec4, vec4)
import Math.Vector4 as V4


{-| Quaternion type
-}
type alias Quat =
    Vec4


{-| Creates a new quaternion with the given x, y, z, and w values.
-}
quat : Float -> Float -> Float -> Float -> Quat
quat =
    V4.vec4


{-| The identity quaternion
-}
identity : Quat
identity =
    quat 0 0 0 1


{-| Creates a new quaternion from an axis vector and an angle (in radians).
-}
fromAxisAngle : Vec3 -> Float -> Quat
fromAxisAngle axis angle =
    let
        halfAngle =
            0.5 * angle

        unitAxis =
            V3.normalize axis

        ( x, y, z ) =
            V3.toTuple (V3.scale (sin halfAngle) unitAxis)
    in
        quat x y z (cos halfAngle)


{-| Extract the x component of a quaternion.
-}
getX : Quat -> Float
getX =
    V4.getX


{-| Extract the y component of a quaternion.
-}
getY : Quat -> Float
getY =
    V4.getY


{-| Extract the z component of a quaternion.
-}
getZ : Quat -> Float
getZ =
    V4.getZ


{-| Extract the w component of a quaternion.
-}
getW : Quat -> Float
getW =
    V4.getW


{-| Update the x component of a quaternion, returning a new quaternion.
-}
setX : Float -> Quat -> Quat
setX =
    V4.setX


{-| Update the y component of a quaternion, returning a new quaternion.
-}
setY : Float -> Quat -> Quat
setY =
    V4.setY


{-| Update the z component of a quaternion, returning a new quaternion.
-}
setZ : Float -> Quat -> Quat
setZ =
    V4.setZ


{-| Update the w component of a quaternion, returning a new quaternion.
-}
setW : Float -> Quat -> Quat
setW =
    V4.setW


{-| Convert a quaternion to a tuple.
-}
toTuple : Quat -> ( Float, Float, Float, Float )
toTuple =
    V4.toTuple


{-| Convert a quaternion to a record.
-}
toRecord : Quat -> { x : Float, y : Float, z : Float, w : Float }
toRecord =
    V4.toRecord


{-| Convert a tuple to a quaternion.
-}
fromTuple : ( Float, Float, Float, Float ) -> Quat
fromTuple =
    V4.fromTuple


{-| Convert a record to a quaternion.
-}
fromRecord : { x : Float, y : Float, z : Float, w : Float } -> Quat
fromRecord =
    V4.fromRecord


{-| Convert a quaternion to a rotation matrix.
-}
toMatrix : Quat -> Mat4
toMatrix q =
    let
        ( x, y, z, w ) =
            toTuple q

        n =
            x * x + y * y + z * z + w * w

        s =
            if n == 0 then
                0
            else
                2 / n

        xx =
            s * x * x

        xy =
            s * x * y

        xz =
            s * x * z

        yy =
            s * y * y

        yz =
            s * y * z

        zz =
            s * z * z

        wx =
            s * w * x

        wy =
            s * w * y

        wz =
            s * w * z

        i =
            vec3
                (1 - (yy + zz))
                (xy + wz)
                (xz - wy)

        j =
            vec3
                (xy - wz)
                (1 - (xx + zz))
                (yz + wx)

        k =
            vec3
                (xz + wy)
                (yz - wx)
                (1 - (xx + yy))
    in
        M4.makeBasis i j k


{-| Quaternion addition: a + b
-}
add : Quat -> Quat -> Quat
add =
    V4.add


{-| Quaternion subtraction: a - b
-}
sub : Quat -> Quat -> Quat
sub =
    V4.sub


{-| Quaternion multiplication: a * b
-}
mul : Quat -> Quat -> Quat
mul q1 q2 =
    let
        ( x1, y1, z1, w1 ) =
            toTuple q1

        ( x2, y2, z2, w2 ) =
            toTuple q2
    in
        quat
            (w1 * x2 + x1 * w2 - y1 * z2 + z1 * y2)
            (w1 * y2 + x1 * z2 + y1 * w2 - z1 * x2)
            (w1 * z2 - x1 * y2 + y1 * x2 + z1 * w2)
            (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)


{-| Quaternion negation: -a
-}
negate : Quat -> Quat
negate =
    V4.negate


{-| Quaternion conjugate: a*
-}
conjugate : Quat -> Quat
conjugate q =
    let
        ( x, y, z, w ) =
            toTuple q
    in
        quat (-x) (-y) (-z) w


{-| The length of the given quaternion: |a|
-}
length : Quat -> Float
length =
    V4.length


{-| The square of the length of the given quaternion: |a| * |a|
-}
lengthSquared : Quat -> Float
lengthSquared =
    V4.lengthSquared


{-| A unit quaternion with the same direction as the given quaternion: a / |a|
-}
normalize : Quat -> Quat
normalize =
    V4.normalize


{-| Multiply the quaternion by a scalar: s * v
-}
scale : Float -> Quat -> Quat
scale =
    V4.scale


{-| The dot product of a and b
-}
dot : Quat -> Quat -> Float
dot =
    V4.dot
