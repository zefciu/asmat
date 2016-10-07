import List 

type Declension = FirstDeclension

vMarkers subject =
    case subject.person of
        ThirdPerson
        

normalPersonMarkers : Screeve -> PersonNumber -> Verb
normalPersonMarkers screeve subject verb =
    if member
        |> screeve series
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
        sfxp +
        plural
