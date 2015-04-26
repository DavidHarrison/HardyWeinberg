module Evolve
    ( Allele
    , Genotype
    , Generation
    , Fitness
    , AlleleModel
    , GenotypeModel
    , Dist
    , init
    , live
    , alleleDist
    , genotypeDist
    , resize
    ) where

import Random exposing (Generator, customGenerator, generate)
import Random.Extra as Random

import Monoid exposing (Avg, avgMonoid, avgPoint, foldMap, runAvg, tupleMonoid)
import NormalDistribution as ND exposing (NormalDist)
import RandomUtil as RU
import TupleUtil exposing (dimap, dup, mapFst)

-- Types ----------------------------------------------------------------------
-- the allele is either dominant (True) or recessive (False)
type alias Allele = Bool
dominant : Allele
dominant = True
recessive : Allele
recessive = False
-- a genotype is a pair of genotypes
type alias Genotype = (Allele, Allele)
homdom : Genotype
homdom = (dominant, dominant)
hetero : Genotype
hetero = (dominant, recessive) -- NOTE: do not use for equality comparison
homrec : Genotype
homrec = (recessive, recessive)

-- a generation of a population is an array of individuals' genotypes
type alias Generation = List Genotype

-- survival : chance of survival [0,1]
-- reproductive : number of viable offspring (>= 0)
type alias Fitness = { survival : Float, reproductive : NormalDist }

type alias AlleleModel a = { dom : a, rec : a }
alleleModelMap : (a -> b) -> AlleleModel a -> AlleleModel b
alleleModelMap f {dom, rec} = { dom=(f dom), rec=(f rec) }

allele : Allele -> AlleleModel a -> a
allele al model = case al of
    True  -> model.dom
    False -> model.rec

type alias GenotypeModel a = { homdom : a, hetero : a, homrec : a }
genotypeModelMap : (a -> b) -> GenotypeModel a -> GenotypeModel b
genotypeModelMap f {homdom, hetero, homrec} =
    { homdom=(f homdom), hetero=(f hetero), homrec=(f homrec) }

genotype : Genotype -> GenotypeModel a -> a
genotype geno model = case geno of
    (True,True)   -> model.homdom
    (False,False) -> model.homrec
    (_,_)         -> model.hetero

type alias Dist = Float

-- Simulation Logic -----------------------------------------------------------
init : Int -> AlleleModel Dist -> Generator Generation
init genSize dist =
    let alleleGen = RU.chooseGen dist.dom (True,False)
        genotypeGen = Random.map2 (,) alleleGen alleleGen
    in Random.list genSize genotypeGen

live : GenotypeModel Fitness -> Generation -> Generator Generation
live fit gen =
    let surFit : GenotypeModel Float
        surFit = genotypeModelMap .survival fit
        repFit : GenotypeModel NormalDist
        repFit = genotypeModelMap .reproductive fit
        gen' : Generator Generation
        gen' = survive surFit gen
    in Random.flatMap (reproduce repFit) gen'

-- TODO: do we need to shuffle?
survive : GenotypeModel Float -> Generation -> Generator Generation
survive fitness gen =
    let survival : Genotype -> Generator Bool
        survival geno =
            let survives n = n < genotype geno fitness
            in Random.map survives (Random.float 0 1)
    in customGenerator <| \seed ->
        let iteratee geno (gs, s) =
            let (survive, s') = generate (survival geno) s
            in if survive then (geno :: gs, s') else (gs, s')
        in List.foldr iteratee ([],seed) gen

reproduce : GenotypeModel NormalDist -> Generation -> Generator Generation
reproduce fitness gen =
    let mates : List Genotype -> List Genotype -> Generator Generation
        mates fs ms =
            let offspringGens = List.map2 (mate fitness) fs ms
            in Random.map List.concat <| Random.flattenList <| offspringGens
    in Random.flatMap (uncurry mates) (RU.divideGen 0.5 gen)

mate : GenotypeModel NormalDist -> Genotype -> Genotype -> Generator (List Genotype)
mate fitness f m =
    let offspring = Random.map2 (,) (RU.chooseGen 0.5 f) (RU.chooseGen 0.5 m)
        fitGens = ND.toGen2 (genotype f fitness) (genotype m fitness)
        num = Random.flatMap (uncurry RU.avgIntGen) fitGens
    in Random.flatMap (\len -> Random.list len offspring) num

alleleDist : GenotypeModel Dist -> AlleleModel Dist
alleleDist {homdom, hetero, homrec} =
    let p = 2 * homdom + hetero
        q = 2 * homrec + hetero
    in { dom=p, rec=q }

genotypeDist : Generation -> GenotypeModel Dist
genotypeDist gen =
    let distAvgMonoid = tupleMonoid avgMonoid avgMonoid
        btoavg = avgPoint << toFloat << btoi
        mappee = dimap (btoavg << uncurry (&&)) (btoavg << uncurry nor) << dup
        (hd,hr) = dimap runAvg runAvg <| foldMap distAvgMonoid mappee gen
    in { homdom=hd, hetero=(1 - hd - hr), homrec=hr }

resize : Int -> Generation -> Generator Generation
resize size generation =
    let lengthDiff = size - List.length generation
    in if (lengthDiff <= 0)
       then
           Random.constant <| List.take size generation
       else
           let genDist = genotypeDist generation
               nonHetero = genDist.homdom + genDist.homrec
               floatToGenotype f = if | f < genDist.homdom -> homdom
                                      | f < nonHetero      -> homrec
                                      | otherwise          -> hetero
               genotypeGen = Random.map floatToGenotype (Random.float 0 1)
               newGenotypes = Random.list lengthDiff genotypeGen
           in Random.map (\gs -> gs ++ generation) newGenotypes

-- Utility Functions ----------------------------------------------------------

nor : Bool -> Bool -> Bool
nor a b = (not a) && (not b)

btoi : Bool -> Int
btoi n = if n then 1 else 0
