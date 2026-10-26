module PowerpuffGirlsSpec (spec) where

import Data.List (nub)
import PowerpuffGirls
import Test.Hspec
import Test.QuickCheck

-- | Generator for arbitrary (but well-formed) people.
genPerson :: Gen Person
genPerson =
  Person
    <$> elements ["Blossom", "Bubbles", "Buttercup", "Silico", ""]
    <*> chooseInt (0, 200)
    <*> listOf (elements ["speed", "bubbles", "super strength"])
    <*> listOf (elements ["Blossom", "Bubbles", "Silico", "Mr. Pig"])

genCity :: Gen City
genCity = City <$> elements ["Townsville", "Pallet Town"] <*> chooseInt (0, 1000)

genThreat :: Gen Threat
genThreat = elements [mojoJojo, princess, gangreneGang]

spec :: Spec
spec = do
  describe "Part 1 - potentialDamage" $ do
    it "Mojo Jojo has a potential damage of 64" $
      potentialDamage mojoJojo `shouldBe` 64
    it "Princess has a potential damage of 89" $
      potentialDamage princess `shouldBe` 89
    it "Gangrene Gang has a potential damage of 40" $
      potentialDamage gangreneGang `shouldBe` 40

  describe "Part 1 - canAttack" $ do
    it "Mojo Jojo can attack Townsville" $
      townsville `shouldSatisfy` canAttack mojoJojo
    it "Princess can attack Townsville" $
      townsville `shouldSatisfy` canAttack princess
    it "Gangrene Gang cannot attack Townsville" $
      townsville `shouldNotSatisfy` canAttack gangreneGang

  describe "Part 1 - canDefeat" $ do
    it "the translated purposes keep the original (even) length parity" $
      map (even . length . purpose) [mojoJojo, princess, gangreneGang] `shouldBe` [True, True, True]
    it "Blossom can defeat Mojo Jojo" $
      blossom `shouldSatisfy` canDefeat mojoJojo
    it "Bubbles cannot defeat Princess (30 is not above 44.5)" $
      bubbles `shouldNotSatisfy` canDefeat princess

  describe "Part 1 - isHighLevel" $ do
    it "Princess is a high-level threat" $
      princess `shouldSatisfy` isHighLevel
    it "Gangrene Gang is not a high-level threat" $
      gangreneGang `shouldNotSatisfy` isHighLevel
    it "a threat weak to kryptonite is never high level (regression: the original misspelled it)" $
      princess {weaknesses = ["bubbles", "kryptonite"]} `shouldNotSatisfy` isHighLevel

  describe "Warm-up" $ do
    it "Princess is invulnerable for Buttercup" $
      buttercup `shouldSatisfy` isInvulnerableFor princess
    it "Mojo Jojo is not invulnerable for Bubbles (speed)" $
      bubbles `shouldNotSatisfy` isInvulnerableFor mojoJojo
    it "the predominant threat is Princess" $
      fmap purpose (predominantThreat [mojoJojo, princess, gangreneGang]) `shouldBe` Just (purpose princess)
    it "there is no predominant threat in an empty list" $
      fmap purpose (predominantThreat []) `shouldBe` Nothing
    it "Blossom and Buttercup can defeat Mojo Jojo" $
      namesOfDefeaters [blossom, bubbles, buttercup] mojoJojo `shouldBe` ["Blossom", "Buttercup"]
    it "B-named girls with more than one skill" $
      map name (bNamedAndMultiSkilled [blossom, bubbles, buttercup, msBellum, mrPig, silico])
        `shouldBe` ["Blossom", "Bubbles", "Buttercup"]
    it "an empty name does not crash the B check" $
      bNamedAndMultiSkilled [blossom {name = ""}] `shouldBe` []

  describe "Going to the nutritionist - 1" $ do
    it "Blossom has no resistance after substance X" $
      resistance (substanceX blossom) `shouldBe` 0
    it "Bubbles has no resistance after substance X" $
      resistance (substanceX bubbles) `shouldBe` 0
    it "Buttercup has no resistance after substance X" $
      resistance (substanceX buttercup) `shouldBe` 0
    it "Bubbles drinks a beer with Silico and ends up with 2 friends" $
      friends (beer ["Silico"] bubbles) `shouldBe` ["Ms. Bellum", "Silico"]
    it "Bubbles drinks a beer with Ms. Bellum and keeps a single friend" $
      friends (beer ["Ms. Bellum"] bubbles) `shouldBe` friends bubbles
    it "nobody befriends herself (regression: the original allowed it)" $
      friends (beer ["Bubbles"] bubbles) `shouldBe` friends bubbles

  describe "Going to the nutritionist - 2" $ do
    it "Buttercup drinks fernet and learns Barbecue Chef" $
      skills (fernet buttercup) `shouldBe` ["speed", "super strength", "Barbecue Chef"]
    it "fernet does not repeat the skill" $
      skills (fernet (fernet buttercup)) `shouldBe` skills (fernet buttercup)
    it "a Strawberry flavouring lowers resistance by 10 letters" $
      resistance (flavouring "Strawberry" blossom) `shouldBe` 45

  describe "Going to the nutritionist - 3" $ do
    it "Buttercup stays the same after gatorei" $
      resistance (gatorei buttercup) `shouldBe` resistance buttercup
    it "Blossom ends with 65 resistance after gatorei" $
      resistance (gatorei blossom) `shouldBe` 65
    it "Blossom drinks 3 shots of vodka and is renamed Blos" $
      name (vodka 3 blossom) `shouldBe` "Blos"
    it "too many shots leave an empty name" $
      name (vodka 100 blossom) `shouldBe` ""

  describe "Going to the nutritionist - 4" $ do
    it "Bubbles has 20 resistance after liquid caramel" $
      resistance (liquidCaramel bubbles) `shouldBe` 20
    it "Ms. Bellum has 0 resistance after liquid caramel" $
      resistance (liquidCaramel msBellum) `shouldBe` 0
    it "Mr. Pig stays the same after cocucha" $
      skills (cocucha mrPig) `shouldBe` skills mrPig
    it "Bubbles drinks cocucha and loses her speed" $
      skills (cocucha bubbles) `shouldBe` ["bubbles"]

  describe "My favourite villain" $ do
    it "Mojo Jojo attacks Townsville: final population 9" $
      population (attack mojoJojo townsville) `shouldBe` 9
    it "Princess attacks Townsville: final population 13" $
      population (attack princess townsville) `shouldBe` 13
    it "Gangrene Gang attacks Townsville once: final population 17" $
      population (attack gangreneGang townsville) `shouldBe` 17
    it "Gangrene Gang attacks Townsville twice: final population 26" $
      population (attack gangreneGang (attack gangreneGang townsville)) `shouldBe` 26
    it "Gangrene Gang attacks Townsville twice: it is renamed Gangrene City" $
      cityName (attack gangreneGang (attack gangreneGang townsville)) `shouldBe` "Gangrene City"

  describe "Let's watch one more (play)" $ do
    it "episode 1 affects Townsville" $
      population (play townsville episode1) `shouldBe` 9
    it "episode 1 does not affect Pallet Town" $
      population (play palletTown episode1) `shouldBe` 10
    it "episode 2 affects Townsville" $
      population (play townsville episode2) `shouldBe` 13
    it "episode 2 does not affect Stardew Valley" $
      population (play stardewValley episode2) `shouldBe` 27
    it "episode 3 affects Stardew Valley" $
      population (play stardewValley episode3) `shouldBe` 19
    it "episode 3 does not affect Pallet Town" $
      population (play palletTown episode3) `shouldBe` 10
    it "episode 4 affects Pallet Town" $
      population (play palletTown episode4) `shouldBe` 12
    it "episode 4 does not affect Townsville" $
      population (play townsville episode4) `shouldBe` 21

  describe "Marathon" $ do
    it "the season leaves Townsville with 1 inhabitant" $
      population (marathon townsville season1) `shouldBe` 1
    it "the season leaves Pallet Town with 12 inhabitants" $
      population (marathon palletTown season1) `shouldBe` 12

  describe "Properties" $ do
    it "substance X always leaves resistance at 0" $
      forAll genPerson $ \person -> resistance (substanceX person) === 0
    it "beer is idempotent" $
      forAll genPerson $ \person ->
        forAll (listOf (elements ["Silico", "Bubbles", "Mr. Pig"])) $ \mates ->
          beer mates (beer mates person) === beer mates person
    it "beer never adds duplicated friends nor the girl herself" $
      forAll genPerson $ \person ->
        forAll (listOf (elements ["Silico", "Bubbles", "Blossom"])) $ \mates ->
          let added = drop (length (friends person)) (friends (beer mates person))
           in added === nub added .&&. notElem (name person) added
    it "vodka removes exactly min shots (length name) letters" $
      forAll genPerson $ \person ->
        forAll (chooseInt (0, 20)) $ \shots ->
          length (name (vodka shots person)) === max 0 (length (name person) - shots)
    it "an attack never leaves a negative population" $
      forAll genThreat $ \threat ->
        forAll genCity $ \city -> population (attack threat city) >= 0
    it "marathon of a concatenated season = marathon of each part in turn" $
      forAll genCity $ \city ->
        forAll (sublistOf season1) $ \firstPart ->
          forAll (sublistOf season1) $ \secondPart ->
            marathon city (firstPart ++ secondPart) === marathon (marathon city firstPart) secondPart
