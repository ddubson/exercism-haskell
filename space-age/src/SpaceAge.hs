module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

orbitalOffset :: Planet -> Float
orbitalOffset Earth = 1
orbitalOffset Mercury = 0.2408467
orbitalOffset Venus = 0.61519726
orbitalOffset Mars = 1.8808158
orbitalOffset Jupiter = 11.862615
orbitalOffset Saturn = 29.447498
orbitalOffset Uranus = 84.016846
orbitalOffset Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet seconds = ageByOffset . orbitalOffset $ planet
    where
        earthSecs = 31557600
        ageByOffset = (seconds /) . (*earthSecs)

