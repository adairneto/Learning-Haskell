module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthAge :: Float -> Float
earthAge = (\x -> x/31557600)

earthTo :: Planet -> Float
earthTo planet = case planet of
	Mercury -> 0.2408467
	Venus -> 0.61519726
	Earth -> 1
	Mars -> 1.8808158
	Jupiter -> 11.862615
	Saturn -> 29.447498
	Uranus -> 84.0146846
	Neptune -> 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet seconds = earthAge seconds / earthTo planet
