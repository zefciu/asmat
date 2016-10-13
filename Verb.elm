import List 

type Declension = FirstDeclension

type Person = FirstPerson | SecondPerson | ThirdPerson

type Screeve = Present

type Series = PresentSeries | FutureSeries | AoristSeries | PerfectSeries

series : Screeve -> Series

series Present = PresentSeries

type alias Verb = {
    preverb: string,
    root: string,
    suffix: string
}

vMarkers personnumber =
    let
        prefix = case personnumber.person of
            FirstPerson -> "ვ"
            _ -> ""
        postfix = if personnumber.plural && personnumber.person != ThirdPerson then "" else "თ" 
    in
        (prefix, postfix)

screeveMarker screeve subject = 
    let def, sing3, pl3 = case screeve of
        Present -> ("", "ს", "ენ")
    in 
        case subject.person, subject.plural of
            (ThirdPerson, false) -> sing3
            (ThirdPerson, true) -> pl3
            _ -> def
        

normalPersonMarkers : Screeve -> PersonNumber -> Verb
normalPersonMarkers screeve subject verb =
    if member 
        |> series screeve
        |> [PresentSeries]
    then
        vMarkers subject

personMarkers : Screeve -> PersonNumber -> Verb
personMarkers screeve subject verb = 
    if member
        |> declension verb 
        [FirstDeclension]
    then
        normalPersonMarkers screeve subject verb


decline : Screeve -> PersonNumber -> Verb
decline screeve subject verb =
    let
        (pfxp, sfxp, plural) = personMarkers screeve subject verb
    in
        (preverb screeve verb) +
        pfxp +
        version verb +
        root screeve verb +
        suffix screeve verb +
        screeveMarker screeve subject +
        sfxp +
        plural
