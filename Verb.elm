module Verb where
import List 

type Declension = FirstDeclension

type Person = FirstPerson | SecondPerson | ThirdPerson

type Screeve = Present

type Series = PresentSeries --| FutureSeries | AoristSeries | PerfectSeries

series : Screeve -> Series

series Present = PresentSeries

type alias Verb = {
    preverb: String,
    version: String,
    root: String,
    suffix: String,
    declension: Declension
}

preverb screeve verb  = verb.preverb

suffix screeve verb = verb.suffix

root screeve verb = verb.root

version screeve verb = verb.version

type alias PersonNumber = {
    person: Person,
    plural: Bool
}

vMarkers personnumber =
    let
        prefix = case personnumber.person of
            FirstPerson -> "ვ"
            _ -> ""
        postfix = if personnumber.plural && personnumber.person /= ThirdPerson then "" else "თ" 
    in
        (prefix, "", postfix)

screeveMarker screeve subject = 
    let (def, sing3, pl3) = case screeve of
        Present -> ("", "ს", "ენ")
    in 
        case (subject.person, subject.plural) of
            (ThirdPerson, False) -> sing3
            (ThirdPerson, True) -> pl3
            _ -> def
        


normalPersonMarkers : Screeve -> PersonNumber -> Verb -> (String, String, String)
normalPersonMarkers screeve subject verb = case series screeve of
    PresentSeries -> vMarkers subject


personMarkers : Screeve -> PersonNumber -> Verb -> (String, String, String)
personMarkers screeve subject verb = case verb.declension of
        FirstDeclension -> normalPersonMarkers screeve subject verb


decline : Screeve -> PersonNumber -> Verb -> String
decline screeve subject verb =
    let
        (pfxp, sfxp, plural) = personMarkers screeve subject verb
    in
        (preverb screeve verb) ++
        pfxp ++
        version screeve verb ++
        root screeve verb ++
        suffix screeve verb ++
        screeveMarker screeve subject ++
        sfxp ++
        plural
