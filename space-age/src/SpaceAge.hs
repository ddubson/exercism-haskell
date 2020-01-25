module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
    Earth -> ageByOrbitalPeriodOffset $ 1
    Mercury -> ageByOrbitalPeriodOffset $ 0.2408467
    Venus -> ageByOrbitalPeriodOffset $ 0.61519726
    Mars -> ageByOrbitalPeriodOffset $ 1.8808158
    Jupiter -> ageByOrbitalPeriodOffset $ 11.862615
    Saturn -> ageByOrbitalPeriodOffset $ 29.447498
    Uranus -> ageByOrbitalPeriodOffset $ 84.016846
    Neptune -> ageByOrbitalPeriodOffset $ 164.79132
    _ -> 0.0
    where
        earthSecs = 31557600
        ageByOrbitalPeriodOffset = (seconds /) . (*earthSecs)

