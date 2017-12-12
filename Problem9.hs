{-# LANGUAGE QuasiQuotes #-}

module Problem9 where

import Data.List (foldl')
import Text.RE.Replace
import Text.RE.TDFA.String

uncancel, ungarbage :: String -> String
uncancel = replaceAll "" . (*=~ [re|!.|])
ungarbage = replaceAll "" . (*=~ [re|<[^>]*>|])

solve9, solve9' :: String -> Int

solve9 = snd . foldl' score (0, 0) . filter (/=',') . ungarbage . uncancel
  where score (depth, total) '{' = (depth+1, total)
        score (depth, total) '}' = (depth-1, total+depth)

solve9' = sum . map length . getCaptures . (*=~ [re|<$([^>]*)>|]) . uncancel
  where getCaptures = map (captureText [cp|1|]) . allMatches

input9 :: String
input9 = "{{{{{{{{<!!!>!>!!oiau!>}}<u!!!!,!!,e!!!>'>}},{}}},{{{{},{}},{{<<<\"!!!>!!,,}!!!>'>}},{<!!{e'a!!u!!u!!!!!oa'!!u!>!!eu!!!!!>,<!>},<,>,<}{!!u,!!!>},<!!!>!!!>>}},{{<!}aa!!{'a!>},<!>,<!>!>,<!u,\"aae!>},<>,<!!!>!>u,o!>},<!!!>!!!>,<\"<'>},{{<,!>},<ae{>},<<e!>},<a!!},}>}},{{<>,{<!!!>!>{!>},<!!!!!>},<!>},<!!!>,<!>,<!!o,<,>}},{<{'>,{{<\"{,<!!!>!>!!!>u!>},<ue!!!!u!!'uua}!!!>},<>}}},{<'e!!>}}},{{{{},<!>},<!!o\"'!>,<u\"<!!<,o{'a>},{{}},{{},{{<'!!i<>},<\"!!!!,!>{{>},{<i'<{i>}}},{{{<!!o!!a!!'!>'>},<eu!>!}\"!>,<}i>},{{<!>},<oe!>},<,'!!!>,<i!>,<!>,<a!!!>!}!>,<\">},{}},{{<e<!>!!o!!!!!>},<,>}}},{{{<uu,'>,{{<!!!>u!!>}}},{},{{{<ei>},<{!!{!!!>!,\"}!!e'!>}u\"a>},{{{<!>!!!>a!><!i!>},<\",!a!!a'\"!>,<'>},{<!!!!!!i<!>},<ie!!!>e{'!\"!>,<<!!o<>}}},{<!>},<!!!!!>!!!,,>,<{!>!!!>a!!>}}}}},{{{},{<!,e}!>,<!>,<!>,!!e{{!oi>,{}}},{{<e!>},<!!\"!>!>e!!io!>,<>,{{<!>},<!>{!>!>},<!i!!'!>!!!e!>},<{\">,{<o!>,<!!!>,<\"!\"!o!!a!>e!!!>a!>!'a>}}}},{<!>,<!!!!u\"a!!'iu'<!!<ui!>{>}},{{},{{<e!!!>!!!>!>},<!><!!!>>},{<e!>>}},{{<!\"eo!eo>}}},{{{{}}},{{<\"'!e,!!ou!!!>!><{!\"i>},<!>},<{>},{<{!!e{ui!!!}i>}}},{{{{{{<!>,<!>,<!>},<>}}},<}!u\"!>!!!>!!e!>i}u!>!!!>,!>,<>},{{<,o!>,<!!!>i!!!>,!>!!!>'}!>i!>,<<!!!>,<<!'>},{{<u!!'!!!>,<>}}},{{<u!!!>,<!!oae!i,!>,<a}!!\",!!,}{>},{<'{!>!!<!!!!eae}!!!!!'>,<}i'!!!>!!'}<<!!'>},{{<>,{<!>},<!!!>,<!>},<!!o!!!>o'aeo,<!>},<o!!!>>}},{<>}}}},{{{},{<<ui!>{!!!>,<!>,<!!!>>}},{<!o'!>\"!>},<,!!!>'!!}>,{}},{{<{u!>,<!!}}ei!>o<'!!!!e<,>}}},{{<!!u!>!!!>i,e!u}ooao!!,!>{<!{>,{{<>}}}}}},{{{{{{{<'\"!!!>!>},<!!!>'eu!>{<<!>},<!>},<>},{<}!!!>e!>,<u{{!!!>o!>,<'>}},{{<!e!'u!!}!}a\"!>e,\"!!i!!!>!>},<>},<u<!>{\"i\"e{!!!>>}},{{<!>!!!>e}!>,<!>},<'i<<!!!>o\"\"!>!!>,<u!>,<,!!!!!>!!!>},<!!}u!>!>},<!!!!!>},<!i!>},<>},{{},{<!!!>!>},<{iu>}}}},{{{{<a!>},<{!i,!>},<!>},<i!>,<{oaa!e!>,<>}},{<uo!!,!>,<!!!!!>>}},{<,!!!!!>!!\"}!!,i\"<i!!!!uei>,<{'!>},<}{!>i<>}},{{{{}},{{}},{{{{<\"!!o'u!!!>,e!>,<,ue!!!>!!!>!!!>!!<!>>}},{<a!!!>!>,<!>!>,<!>ai!!>,{{}}},{<!>,<!>,<>,<a>}},{{<{!!!>},<!!a!!!>!!!>,<!!!>!,!!!>!>},<>,{<!>!'!!'>}},{}}}},{{{{<!>i,}i,!!!i,<>},<!>,<!ae!!!>{ua}!a!!o!>!>!><>},{{<o\"!>uo{\"!!au!!u,!<a\"!!{o>},<!!!>!!!!!>!!!!!>!!!!!o!!}!>o>},{{{},{{<\"<!>},<!!!>!!o!>},<>}},{<,!>},<!!!>u!!>}},{{{<\"<>},<!!>},{<!>>,<'!!\"'}{a!>},<>}},{<ue}a'!>!!!>,<!!!>!!!>!'\"!!!>},<!<}!>},<>}}},{{{<!!,ia!!<!!!>!{uo!,>}},<,!>,<!!e!!!!!><oo!!ii'!!!!\"'<a!>},<<>},{{}}},{{{<!>},<}!!!>a>},{<!>a,!>},<!!!!,e!>,<\",!\"\",!!!>ie!>!!{!!\">}},{{{{<ea!!!>,<!>},<,!{>},<\",!>},<!>!>},<o}!!!>!!!!'!!!{!>,<{!!\"!!u!!}>},{<u!!}!a}u!>,<!!a!>},<{i\"a>,{<{!!!>u!!!!!!!>!!!>,e>}},{<u!!!>},<!><'>,<<!>,<!!{u!!>}},{<o!i{!!!>!!!!e!>},<!<>,<oa!>,<a\"'!<i}a>},{<i!u!!!!!!!>!!{}!>!,!<,>,{<u!!!>!!'!!!>},<!<i>}}}},{{},{<!e!!u>}}},{{<{!!!>!>},<a!!!>>},{{<!!!>!!,a\"e!i!>!!!>!!!!i\"!!!!!>iie>,{}},<\"{u!,,>}}},{{{<a>,<}o>},{},{{{<!!u>,<!>,<!!u>},{{},{<!>},<!>,<!eee,!!!>},<<!!!>{a'>}}},{{{<a<,i'e!>},<'\"iu'o!>},<!!!>,<!!!>},<>},{<!!\"}!>o!,'!!<uee!>,<!!!>},<io!'!!!!{e>,<,!!{'\"!!!>>}}}}},{{<!>},<e!!{'{!!o\"!!!>>,<!>},<!!'!>u!<>},{{{{{<\"!!\">}}}}}},{{{<u!>!>},<u>}}}},{{<!>},<o!eea\"<<{\"\"!!!>>},{<u\"e!!!>,<,!!u>,{<{!>},<!!!>e'}e!!u!>o!!!!ui>}}},{{<ii,!>,<a!>},<!>},<!!{u>,{{<a'a'!!iee{!!!>!!!!!>!>i'o!!'!!!>!!}>},<{}>}},{{}}}},{{{<!>,<,!!u!'\"!>},<!>i>,{}}},{{<{<!!!!!!i!!iii'o!!{'iao!}>},<>}},{{{},{{<\"i'!>},<,!o!>,i>}}}},{{{{{{<i!!!!!!!>\"\",!!!>\"o!,>},<aa!>!>},<i!auo\"!\"!{{!>},<,o>},<!>},<!e!!!>o!!i!>,<!!!>!!!>u\"!>,<a'!>},<{!>},<!>\"!!!>>},{<!!!!o,!!,a!!!!!>,<!!!>},<!<,!>!>,<!!!>,\"e!!!>>,<'!>,<}!o!!!u'u,}!<eu{}!>},<!>},<}>},{{{<\"!>{!>,<!>,<{!!o!>},<!!a},!>},<a,a>}}}}}}},{{{{{{{<!>o!>!>},<!!!>i!!!>ei<<!>,<,o!!!>,<!>},<<>,<u!!}>},{{{{}}},<e!>!>!!eio,>},{{{}},{{},{{<!>},<!>},<!i!i!a!!}\"<}<>},<e!>},<!!!>,u!>!{o!>},<ee}>},{{<>},{}}}}}},{{<!>,<!!\"!iu!>!!!>\"!!,!!!>!>!!!>},<>,{<o!!o!!!>>}},{{{<>}},{}}},{{<ui!>,<i!!!>\"!>},<<!>},<e!!!>\"!>,<}ui>},{{<!!!!!>\"\"u<{a!>u!>o!>,<,!!!!}o>,{<!!!>,<>}}},{<!>!>},<!!<i!{!!!>!!}>}},{{{<}}>},{<!>},<o{!!!>!!ue!!!>,<i!a!!!>>}},{{<i!!!{a!>,<!!!>\"!>!>!!!!!>}>,{<!>!!!>!!a'!>,<!>,<u!>a!!!!{'<o>}}}}},{{{{},{}}},{<\"a,,!!!>,<!!!>!!!!!!!!!>u<i!{,',,>,{<u\"!!!!!!!>\"!!}!>!!!!{,>}},{<\">,{<e!!!>!>},<o!>!><,,!>},<e!!!!!\"!e'!>>}}},{{{<}\"e!>,<!!\"},>},{}},{{{{{{{<\"!,!>',>},{<!!!>!!u,,!!!u!!!>!{e!u!i\"}u!'a{ae>,{<\"!>,<'!>!>,<!>}!}}{{!!!o!!!!!!!>!!e!!'>}}}},{<!<!!!>u!!<}i!!o!!!>!>,<!>}!'!!!>,<}>}}},{<e<\"}ao!!e!!>},{<}!!\"eao!>},<i!!!>,<'!>\"o!>,<>,<!!u<!>!!!>i!>},<>}},{<!!'}}!>},<!>oa<'>,<{!a!>,<!{{!>},<!>,<o!!!>},<!!}i<!u>}},{{},{<o!u!>,'!!!!!!u!e>,{}},{<u!!!>{!>\"!!e<!>},<!!<!!}!>},<iu!>},<\"e!!a>}}},{{{<<!!{!!!><<!>},<{!!!>!>!!!>'\"!>},<>}},{{{},{<,o!!!>\"'!!i}o>}},{{<!!'\"i!!<>},{<!!}ueau,!!!>'!!\"!!o{!>,<\">}},{{<!!!<'!!}!>,<!>},<e!>,<,!!!!u!'!!!>!!\"u<uia>},{{}}}},{{}}}},{{{<a'>,{}},{<a}e!!,!>,<u>}},{{<!!''e'!!!!!>!{!>},<>}}},{{{<!>,<\"!!!a!!!>!>!>!>},<!><a}}o!>>}},{{{{{}}},{<'!a!>,<i'u!>,<!>u!>!>,<>,<i,u!!!>!!!><u{!o!!!!<o>},{{},<}<!!!<!!!>uoiie!!!>ueo!!!a>}},{{{{},{<>}},{{<!ee!>},<!!!!\"u!>!!!>},<{'a<!!<'>,<<!!!>,<!>!!a!!e<,>},{{<'<>},{{},{}},{<<oe>,{}}},{<!>},<!!,!!!!u,!!e<'!!<e!!!>>}}},{{},<\"!>},<<}!>},<u!!}<!>,<}!>,<>},{<{!!!!!!!>,,!!u!>},<\"\"o!!{!'!!{!>},<i{>,<e<!>!><,!>},<<>}},{<!e\"<{!>},<u{!>!!!>}e!!\"'!>,<!>},<\"a}>,<!>,<!i!!!>!!aa}>}}}},{{{{},<!><!>!>},<!!!!,uio'u<!!!>!>},<!>,<i!!,>},{}},{<!o,!!!>,<!>,i!!i!!!><>},{{<eeaou!!!>},<!!\"!!u}!'!!{!}\"'!>},<!>>},<\"'o!>!!!!o!!o<!!!>!>,<!oa{!a!e!!{,}>}}},{{{}},{{<!>{<eo!eu>,{}},{{<a,!!!!<!!!!}!>,<e>,{<!>},'u{!!!>!>},<!>,<>}},{<<!!!>},<}!>,<u\"a{>,<!!!!!>},!uoui>},{<!>},<>,{{<!e!}ui,<eo!>>}}}},{{<!!!>!>},<{!!!>\"i!!!!!!{>}}}},{{{{{{}},{{{}},{<,o{ia{a!!!>!i!>},<!!!e>,<>}}},{{<!\"}>,{<!>},<{!e!!!>i!!!>e!!!!a!>,<}!}!'!!!><!!!>},<!!'o<!>},<>}}}},{{{{{{}}},{{}}},{}}},{{{{<!!oa!,}!ie!>},<<>,<a'!!,!>e!\"{!><>}},{<!!!>}a!!!>{!!a!!!!!>\"!>},<oeeai!!i\">}},{{<e!!au!!!!oa!!!!\"!>},<!>},<u,!!ao!!<!!!!!>!!'\">},{<'!>>,<}e,!!!>!!!!!!!>\"i!!',!>,<e!!,>},{}},{{{{<,o!>,<,'<io!!!>},<i{!!{>}},{{},{<}}!!!o!>!>},<!!ae!>ee'!>}!>,<ie!!!!\">}}},{{{{<u,!!!>{,!!aao!>,<>},<!!,{}!>},<!>},<!!u!!!>!!!!a,a!>,<!>,<>},{{<!!i!>},<\"}!!!>!!!>!{i\"!!>},<!>!i!!!>}e!!<'o{>},{<>,{{<'!>!>},<i!>},<{a!>,<!i>},<u!>},<i!>'!>},<!>,<,!>},<!!uu>}}},{{<<!>},<{!!a,a>},<,ue>}}}},{{{{<!!io,!!!>>,{<i}}ui{!!u>}},<!>},<>},{{<e!!\"!!!\"!!!>!>},<!!u\"!}<!!oi!o}a!!!><!>>}},{{<!>},<'!>!>,'!>,<i\"o!!!>{!i!}!>,<\"!>},<!!!!{>,{<<{,!e!!u'!!u'!>,<a!>,<i!o,!!!>{<>}}}},{{{},{{<!>,<<e,>},{<!!!!!>,'a,',<!!\"!e!,u,>,{<,u!o,!!<,\"oea'!!!>!>},<{',<>}}},{{<>}}}},{{<!>,<!>,e!>!!<!!!>i!!!>!\"!!!>}!>!>},<}a\"<!>},<>},{{{<>},{}},{<\"!>},<}i!!!!!>'u!>!!!>,i!!!>}i!>i!>,<a>}}},{{{<o!\"!>},<!>},<!!{u!>>,{{<!>,<oi!!!!<!a!!i!>,<>},<!>,<!>},<\"e!>,<\"!!!>!!!>{!!eia!!!><<!!,>}},<>},{{{{{},{<{!!!>},<>}}}}},{<a\"<!>,<!>,<!!!!!>o>}}},{{{<>,<'!!,oa!}eu,!>},<{ea,!>,<!!!>,<\"\">},{{{<!!'u}i!!!>}}!!i!>,<!>,<{!,i<o>},{{},<u}ee!!ai!!<!a!>!o!!o>}},{{<o}o!>},<i!<!!!><!>,<!>,<!!a!u>},{<{a!!!>u}u!}\"!>},<!>,<e!>},<!!!!!>},<}!\"!!}>}},{{{},{<a!>,<<i!!e!!ou,!!!>iu!{oo>}},{{},{<}\"!>!i!>>}}}},{{<,<o!!'<<>}}},{{{{{<!>,<u{!!!>!>,<!!!!!>!!<!!!!i}!!a!>,<{!>},<o!>!!!>!!>,{<!!!>!>>}},{{},{<{,!>},<!!!>!!e!>},<!!!!{!!!>}!<o>}},{<!\"!>,<}!!!!!!!>},<{!!a!>},<a!>u!!''<{>}},{{},{{{{<!!o!!io!!a!!i!>,<\"i<!}<!<i!>},<io>}},{<<!!<\"<!>\"!a!>,<!>},<>}}},{<>,{}}}},{{<u!!!>>,<\"e!!!>u'!>},<'a!>{!>,<i!\"'i>},{<a<!!{!o!!!>>,{}},{{}}}},{{{<}ao!!!>e!>!>,<!!!>!>,<!\"\"a!!!>}}o!!!!!<\">,<}!ia<o!!!!e!>,<>},{<{<!>},<!>>,{}},{{{{{<,ou<<u!',<\"!>,<!!i!!!>}o>}}}},{<oia\"a,i}>,<!!!!!!!>!!!>},<!!a>}}},{},{{{<!>},<!>o\"!>,<!>,<!>},<},<a!eo!!!>\"a<!>},<{>},{{<!}!>,<au!>,<'<o!!!>i!!!>},<}!a\">},<!>,<!!{!!!>}i'u!!!>,<!}!!!!!>'!>!!\">}}},{{{{<i!u}!!!>!>,<e!o{>},<!!!><!>!!o,>},{{}}},{{{<>}},{{{<\"ou{u!}<}\"!>,<i!'!\"!>!!!}!>>,{}},<!!!!!!!>,<!!!!i!>,<!o!>,<<!!!!!>'e!!!>au>},<!!!>,<{}!!!>o!>,<!!}!!a>}},{}}},{{},{{<>},{{<!!!!!>\"!!!><,!!!>o>}}},{{<!!!>u<i!!}}o!>!!o!>a'!!>},{}}},{{<!!!>,<!o!>},<!!!><u!!a},'u'o!a!>,<a!>>,<i'!!u'!!!>a!!,<<!>,<!!!>o!o!!!!!!!>>},{{{<\",<!u!!!>,<u!>>}}},{<i!>!ai!!!i,a!>!!{o\"!!ua!!!>!!<>,<o!!!>,<u!!i\"ui',!>,<\"a!>},<o!!!!!'!>},<>}}},{{},{<<!!',\"!!!>e!!\"{>,{<\"!!\"!>},<!>a{!>},<o\"}!!!>,>}},{{<!!\"eu>,<!>},<!!au!!!!!,\"a<,>},<i!aou,aoe{ee>}},{{}}}},{{{{{{<o!!!{o>},{<>}},{{<u!!!!!!!!!!oa!>},<aaua\"!>},<>},{}}},{{{{{<\"!>'!>},<ui<{oa}!>},<{!>!>},<!,''!!o>},<<\"!>},<!>},<,!!\"!>},<!!>},{}}},{},{{<!>},<\"!!!!!>\"!!,!>},<!>ii!!!><!>\",uo'\">},<,!!ua'!!u\"au!!{<ee!o\"!>},<!>,<!!!>,<>}},{{<!>}o,!!}>,<!>!>},<e!e!>},<,!!!><u!>,<>},{}}},{<<}!>,<a!!!>>}},{{{<}>,<>},{<},i>,{{<!!!>i!>},<!!'e!>},<!!aa\"o<<\"!>},<!!oo>},<,'!!!>eu'!oai'!>,<{!>,<!>},<!!>}}},{<!auuu!>},<aau!!!>!!a>,{<!!\"<}!!\"!>}'!>,<>}},{{},<oa!aa!>,<!!!>},<>}},{{<!!{!>!!{!>,<o!!!'\"!!ea!!!>>},{{<!!!>},<o!>,<!>,<>},{<,{!!>}},{{<{}!>},<e!><e}!!!>},<!!!!!>e>,<!{}aeeoo{>},{{<!!!!!>!>i!!i>},<ei!>},<>}}},{{{{<{e!u{!!''!ie!>},<'!<!!!!,!>,>},{{}}}}}},{{{{},{{{{{},{{<!!o!>,<!!!>e>},<,}\">}}},{{{{<!!!>'eo!!{!!!>\"o''>},{{{{<!!o!!<!><!!!>!!!>,<!e'i<!!!'a'{!!a!'!>,<>}}},{}}},{{<!aa>},{<{i!au,o!<!>,<{>}},{{<!a!!u>},<{'!!\"!!!>>}}},{{<!!!>{uu\"o!!a>}}},{{{{{{<!!u!!'!\">},{}},{},{<>}},{{{<}!!i!!!!aauu'>},<'}!>},<ao!!!>!>},<!!!>oiao}!}!>!!>},{<u'!>},<euo!!!>},<!>,<!!!>{'>},{{<!!!!!>,!>},<!,e!,>},{<!!i!!!>!!!!!!>,{}},{{<!>},<!>},<!!!!!!u!>},<e>,<!{u\"i,i{!!<{e>},{}}}},{{{<}!>,<,o\"'!eu<!!ii>},<!>!>,<!><}}'e!{!!i!o!!!>!>>},{<!>,<{!!!>,a!>!!!!!e,a>}},{{{{<!>},<i!<!>'!!!!o!eii!>,<!>i}i!!!>},<>}},<!!!>!!'!>},<<!!!>\"{eio'!}{<u>},{{{}}},{}}},{{{},{{<'!<>},{}},{{<}!e{e!!!!!>},<\"i!!'!!!!!}\"i!!e\"!>,>},{}}},{<\"!>},<!!!>},<}o',!eoae!!e,u!>,<i!>},<>,{<!!!>ei<!!!>!>},<}eo!!!>!>\"i!!}!!\">}},{{{{}}},{<\"o<!!i!!{!ua!!o>}}},{{{<\"!!!!!>!!!>},<>},<}}!>o<!!<!!!><!}oeoi\"!!i!>,<a!>>}}},{{}}},{{{<<ae}!><!!\"i!!!!e!!!>,<!>!!!>i!>},<,a\">},<>},{{{},{<}}{e{!!a!>},<!!!>>}},{{{<i!>\",!{a>}},{<o!!,a!!!>ou!>},<>}},{{<>}}},{{{<!a!>,<!!}{!!\"o!!!!a'u,!!!!!!uu!>,<!>,<<>}}}},{{{<!!<!>}!!{ua!>,<,e!>,<a!!!>>,{<{,u!>!>},<i}!>,<>}},{{{<!\"a!>,<>},<'i,!>},<{!>},<!>},<!u!>,<oo{!!!{{>},{{<!>},<!>!!u!>},<>,<,o!>,<>},<e\"!!!>'{!>!!<'{!>u>},{}}},{{{<<!}i>}},{{{<u\">}},<a,{!!!>!eae<>}},{{<ioa\"<>,<>}},{{{{<<a,!>u!!!>!>},<'!>,!o!!!><iu!!!>,<i<>},{<}u,!!!>a!!!>>}}}}}},{{{{<}!!o'>}},{}},{{<!!i!>,<!!}a!>},<!>,<\"!!!>,<'!!!,a}!!!!}>},{<ooei!u!!!!!>}}!!a!!!>!!!>,<eu>,<!>},<o!!!>e!!!!!>!!{!!!>!,a!!!!ou!ai{<u>}}},{{{{<o}{{>,{<>}},{{},<{!!!!{!!!>},<}!!!!!!!>!!'u>},{<i{!a{a!!,{}o<\"i>,{<a!!!>!>},<<iui!!{\"<!!!>!>,<,>}}},{{<ue}'{!>!>},<>},{}}},{{{<,<a!>!!!!!!!>{u!!,!>>},{{<!{'!!!!!!i!!!>oo!>},<{!!!>,<a}!>!!ie>}}},{<}!>,\"ao'!>!!!>{a''<aia>,{<u>}}},{{{<\"\"!>},<!!i!>},<\"u>},{<!>},<{\"!!!>{{!!!>a!!oii{!>},<\"!>,<!!o>}},{<\"!!!!!>e!!'{!>},<'>,<u!>},<!!}!>},<e!!!!!!}e!>,<e!!\"!}!!,>},{{<<!>},<'e<\"}>},{<o!i!!!!!>,<!!!>,<,,!>{!!!>,<!!!>,{\"!}>,{{<o!!!>u!>},<ea!>},<u!>},<i>}}}}}}},{{{{},{{<>}},{{<!o!!!!!,!>!>>,<!!o!!'!!ei<e!!!>!!!!i{!!}!!!>},<e''!','>}}},{{}}},{{{<}u!>,<e!!!>!>},<e!'a!>e<!!{>}},{<'!!iu}{u!>,<!i!!!>!>,<!!i!>>,{{<!>,<<!!<a\"o>},{{<!>!>,<i!!}<u>}}}}},{{{<>,{<!!'{!>,<'',{}!>},<>,{{{{<}!!a!!>}},{<!!!!!>>}},<>}}}},{{<{{!>},<>},{},{{<!!,!!!>'!!!>}!<>,{<!!<!!!>ei!!!>!!!>a'\"o!!!>,<a!>!!!><!!aa!!>,{{<oi!!',!}}o!!!!!!eo!>},<!{!!i>},{{{<\"ioa!>},<a!>i}ueo>}},{{}}}}}},{}}}},{{<ao>,<!>},<>},{{<}ao!!!>!>!!>}}}}},{{{{{{{<i!!!>o!>,<!!\"u>}}},<!!!!!>o!!\"u\"'!!!>!>},<!>,<!!!!>},{<u'{!!!>!!!>a,,u!><>}}},{{{{<!!io,!o>}},{{<!!!>i!>,<e!>,<\"i!>!>,<\"u!!>},{<!!u!i>}},{{},<o<!>},<ia>}},{{{{<!>},<'!ua!!!>},<a!!!!}}!!!!\"!'i!!!>u>,{<'!!\"e{'!>},<!!{\"'!>,<a!!!>>}},{{{<>},{<<a!!!>e!!!>u>,{<!!!>,<}<ii!!!>!>},<,'!!!>},<oa!\"iu<>}}},{{{{{<!>,<>}}},{{<,,!!!>iu,!>},<\"i!>,<<i!!,a,ei>,{<!!'!!'i!o!>'!!!o!>},<!!{!>},<{!!o!>},<,>}},{}}},{{<i\"u!>!<\"!>,}!!!>a>},<!e!>},<u!!{!!{uoe{!>,<e!!!!!!!>!!!>>},{{<i},}\",,!>},<<'>}}}},{{{<!>,<!>,<u!!i!!!>{>}},{<!!}o'o>}}},{{{<!!!!,!>},<{!>,<,iae\"!!!!,>}},{<,!>\"\"!!!>'ii{!>,<ouio!>,<!!!>},<}<a>,{<!!!!!>!eu!>},<!!'!!!>!>},<'i<{!,o>}},{<oua!\"\"!>o,,e,>,{<<oi{}!,,o>,<>}}}},{{<!>},<!!\"ee!!!!a!>,<eo>},<o!!!>!>,<!uieu!><!>},<\"!>,<!>},<!>!!!>>}},{{{{{<!!!>},<!>},<}\"i!!!><i!>},<u{}!}\"'{o!>u>}},{{<!>,<{!>},<,u!>},<}!!!>!>!{!!!u!!o!>},<>},<!!,'!>,<!>,<\"!>},<<iu}o{!}!!!>u!!u!!>}},{{}},{{<e'u\"!>},<!ei!>},<!}!>!!!!o!!!>},<eo,!!>}}},{<ou}!!!e{u\"!a!o!>!>\"}!>},<!!!>{>},{{<!!!>,!!u!>,<e<'}<!!!>,<\">},{{<!!u!!!!!>\"i!>},<>}}}},{}},{{},{{},<!!!!!}e!!\"i>},{{{<<eoi>}}}},{{<,<ie'<<>,{<<}!>,<ee!!!>!>},<\"!o<>,{<!!ie>}}}}},{{{<'>},{{{<ou!!!!!euu>}},<,,'!eiu<e{!!'i!>},<!>,>},{}},{{},{{{{{}},{},{{<!>!\"\"<!>},<,<!>,<o'o!!u!!,!!>},{}}},{},{{{<e{<}!!,e<!!!!!>i!!!!{>}}}},{{{<>,{{}}},{{{<!!!!}o{}<u'i!!!>!!!!!i!>},<<>}},<!>,<!!!>!!!>u!>},<{!!{'i}!>,<!>,<,<!>,<!>},<'!!\",>}},{}}},{}},{{{{{{<!>},<!!ua{{o!!u>},{}}},{<a!>au!!!>u!!i<\">}},{},{<!\"u!!!>\"i!>,<,!}ea{!>,<>}},{{{<}!!\"i!!!>!>,<!!!>!!'>}},{<!a!!,!>!>\"',!!!e<\"!!!>,!>},<u>}}},{{{{}}},{{}},{}}},{{{{<<!!!>},<!>,<!!!u',!!eu!!e>},{<'!>,<!>>,<oo!!o!>,<}a!>!'!>},<>}}},{{},{{<e}ai}>},{<\",!>},<!>,<a!>!>u!>}'e<!!!>},<>}}},{{{<!}e\"u,a'!!!!!!!a!>!!!>a,o>,{{<{a{!>},<!!!>>}}},<!!!!'oe,{>}},{{{{<,!!}!!<\"!!{!!'<!\"\"<<u>},{<!!o!>},<}!!!>!>,<ei!<!>},<!!!,i!!!>i!!u!!!>>}},{}}}},{{{{{{<}o!>},<<<i,}e!>,<!>},<!!<!>!>,<io}!>},<}>},{<!!!>},{!eoe!>,!<o!!!>\"a}o>,{}}},{{{{<{!!>},{<u!!!!!>o!!!>!}!!!!e>}},<}!!!>,<a!>,<!'{!,!i}{>}},{{<u!o!>!!!!!>!>},<!e{e!!i{!!!>'i!!'>},{<<oiuaui!>,<\"!}!!!>!!i!e!>!!!>'!!eio>}}}},{{{{<<oo,!,\"!!u,!!>}}},{{<{'ie!!}!!!!e!!!!{!>},<,e'o!}a,<,>},{{<>,<\"!!!!!!!>\"!{<u!>,<!!!>!!!>,<!!!>!!<!!<>}},{{{<e!!!!e!>,<!>,<,!>,<\",!!{{!!<!!>},{{<{',e<<>},{<{o{a'!<{!!!aee!!!!!>!<a!!!'!>{u>}}}}},{<,>,{}}},{{{<!>}>,{<e!,!!!{o!>''!>},<!>},<!>}a<\">}},{<a{o!!{oa!>!>,<'!>!!!>!!!>e!!!>!>,<!>,<>}},{}},{{{{<!>,<!!!!,}<ae!!<\"'\"<o!!\"!!!>},<<>},{<{!!\",\"},}{i\"!!,i!!\"!}!!!>,<>}}},{<u!>\"iu{{\"!>!!!>!>},<>,<!>},<!>,<o!!!>u<u>},{<>,{}}}},{},{{<}!>,<!!!>!!!!e'i!}!!<!!!!!>!!!>}!>,<\">,<o!>,<!!}>},{<\">,{<i!!>,<<u!!!>aa>}},{{<!>!!ee'!!!!'!>!!!>,<!>},<!!!!!!!>>},{{<!>},<u!a}!!!>,<!!!>o'ooa{!>,<{!!!i<\"'>},<!\"!>>},{<e!!!!!>!><>}}}}}}},{{{{{<>},<!>,<o\"o{!>},<}!!!!ia!!!>!!!ua!>!!!>!{\"!>,<o!e>},{},{{{{}},<!,!>!!<,}uo>}}}}},{{{{{{<\"!}!>},<<eee!!!>'!!!>o!!}{}{!!,,!e!!!>>},<!>a\"\"!!!>,<!>,<>},<!!,,!>},<i!!!>!>,<ei!!!>!>,<!!o!<!!!>!!!>}<!>,<<>}},{{{<!>,<!!,\"i}!>},<a!>},<!!!>!!!{!e>},<,}!!,!>i!>},<{!>,<ae!e,<oo!>ui>},{<o!!}o!>ia\"}!!!>!!,!>,<>,{<au,!>},<o>}}}},{{{<{!>,<>,{<'!!i!!!>,<}!!ou{!>!!!eoie{u!>},<!}>}},{{{<}!!a>}}},{<!{{i!>},<!!!>'}!!!>i>,{<{!!!><i!!!>!!!<>}}}},{{{},{<i,!>,<e!!i!>ue!!!>!!}!ui>}},{{{<}!!a!>,<o!a!>,<u,ua!u!u,ai,>}}}}}},{{{{{{},{<}<e!<<>}},{<!\"a\"o!!!>a!>\"!>,<u!!<!>,<!!,,oa>,<o!!{}!>e!>\"!\"'{o}aa>}}},{{{<!!!>aa!!',!>o,!!a>}}}},{{{{{},<,o!!!>!!!>>},{{<!i!'a!!!>},<i!>,<!!}i>},{{}}},{{{<u!>\"!!!>!>,<!!u!!!><{<!>,<}e>,<'e<,!>,<!>!!!>,<'>}}}},{{<{i},!!oue!!!>oo>}},{{{<,e!!e,u<u!!\"\">},<!>!>,<!\"!>},<{e!!!>!>,<i!!!>!!\"!>,<i!!a!!!<uu>},{{{}},<,o!!!>i}!!,'o!!,!,!!!>},<,eu'e>},{{<!>,<}!\">,<io!!!!!>},<'!!a,e{>},<!e,!!!!i!!!<!!!>},<}!!!>,<,!>i!!!>!>,<!!<e!\">}}},{{<!>,<!>},<oo>,{<,,!!!>'o!!<o\"}{>}},{{<<a!!!>},<,!{>},{{<\",oa\"!!!!,!u}!>!>,<>},{<!!a,>}}}},{{{{<!>,<,}i!>,<!>u'{<e}\"a>},<>},{{<<\"!'!!o\"{o{!e<oa{a!i!!\"!>,<!a>},<i!!!>!}!!,!!!>},<<!>,<!>},<u!>!>,<u!>},<>}},{{<!!!>!>,<!!}!!ai!!!!!u!>!>!\"uo{u!!!!!>},<{>},{{{<ai}<o!>a!>},<!!!!i'!>a>}},<>}},{{<a\",{!!u\"}!>!!a!!,!!i!!!>,<!!!>>,<<!!<a<!!!\"!>},<}e!!!>!>},<{!!!>!!!!o>},<e!!<!!<{>}},{{{<a!!<'!>,<!!,!!!!o!>},<>}},{{{<},}!i!>!!!!<\"'!>!!!!!<!>},<!!>}}},{<!>},<'!>,<!>,<!\"\">}}},{{{{},{}},{<<!>},<!!{!!!>o!!!!!>!<i}\"!!ei!>},<!>,<!>},<!>},<e>},{}},{{{},{<>}}},{{<!'<!!!\"<!!!!!>,i!>!>i!>e>},{{{{<euo!>,<io!!,e!>,<>,<uu!!\"u!!{!!!>i!}{!,!>,<a>}}}},{{{{<!>,<!>},<i<,!>},<u'}a,oue!>!!o>}},<a>},{<e!!!>!!ee!>!}!!!!!>!!!>,<!!!>{}\"ua<,!>},<>}}},{{{<!>,<!>,<e!!!><o!>!!!>,<u!>u<o!>},<!!!'}>,{<ia!!!!>}},{<!!\",a!!!>!'!a}!!!>{!>!!}!!ooi!!!>!<>}},{{},{<eao!!!!i!>i!!{!!<e!!!>>,{<ee!!!>},<\"'oa{\"!!\",!!o!>}>}},{{{{}},{{<>},<'{{!>},<a,'!!\"'!!!!>},{{<'e\",>},{<!!{'!u!>,<!,!!!>},<!!a!>!>},<\"!!<!!!>},<!}'>}}},{<!!!>},<!>},<\"!!!><!!!!!>a!<o!!!>'!>aa!>,<!!!!!>},<!>>,<,e!><a!>},<e!oo!!!>o!!'!>},<{>},{{{{},{<!>,<u<!>,<\"<!>!!\"{',<!!!!!>},<<i!>,<>}},{<!>,o!o{u>,{{<u{a!!}>}}}},<<e!!,ee>}}},{{{{},{}},{{<!>},<auu!!!>u}!>,<{{!>},<},}!>,<!><o>,<ii\"uu!>,<uua}}<'aai>}}}}}},{{{},{},{}}}},{{{},{{{{<<!>},<e{oaoo,!!<!!}<!!!><!>,<<\"!!a!>,<>},{},{{},<!!!>\"!!<!!!>!>},<!!a!!o!!!>o!!!>o>}}},{{{{<\"u<!>,<\",!>},<aa!'!!>}}},{{{<<}\"!o\"!!!!e>}},{{{<!>a}'>}},{{{{<>}}},{{{<<i}>}}}}},{{<ei!,a,!>!!<e\"<a'{>,{<!!i!}eeu!>},<\"!a!<,i!!e'i,!!<>}},{{}},{{}}},{{<!!e{{!>},<{<}ei>,{}},{{{<!!!>!!',\"ei!>!!!o!!!!!<'u{!>},<!!\">},{<\"!o!>,<e\"!>,<!>,<>}},{<!>},<i},,!><,!!!>a>,{<!!{}e>}},{{{<{}!ei>},{{<!>,<!!!>io{i!!e!>!!},!!u!>u>}}}}},{{{{}},{<a!e!!}<\"!!!>,<>}}}}},{},{{{{<'{{!!{!uu,}{>},{{<!'e{!>,<ee!!!>!!!{{!e!!!!<a,>}},{{{<!!!>!>uo!>},<!{'i<!!!>!>'<e\">},<{o!!',<>},{}}}}}},{}}},{{{{{<{!!>,{<uo!a!>},<!!i!eio\"!\"!>,<!!i!>,<{>}},{<i!!!>!!!>i!>},<,,}e>,<uoa!!\"!!!>!!!!!>},<iee\"!ai!>},<{{,>},{{{<ui}!o'!!!>!>,<!!!>o!!u>},{<ii,e>}}}},{{},<oi!>,<e,!!!!{!!u!!!>{,!>oi>},{<!!a<}o!>,<o!!!>!!!>!!!>,<{\"!!,o{!>,<\"!'<>,<{!>},<!!!!!!!>>}},{{{<a!!e',!!!>!!!>o!!!!!>ei!>>}},{{<aou'}}!}>},<!>i!!!!!>e,e'!,!>!!i!!u\">},{<!!o!!>,{<e!i'!!!>,<>}}},{{{<!>},<!>,<!!\"{!>,<!!!>'>,{{<!!<!a!!!!!>!!!!!!!>,<!>!>,<,!>>}}}}}},{{{},{{{<ea!!!>},<u,>,<'!!!!!>},<{!}!>},<!}!!e}o>},{<!!u!{!>,<!!>}},{{}},{<>}}},{{<!!!>},<!>i}!!}iou}>,{}},{{}}},{{{},<>},{{<>},{<!>!>},<!>},<\"i\"\"}{oi,o>}},{<!>},<!>iu'<\"!>},<,!>},<!<!!\"!>,<\"a}<!>},<'>,<!>!!!!!>eu!>!>},<!!!>!>{>}},{{<,!!ee\"u!!!>a}!>,>,<\"<\"'\"i!\"!>ei>},{{{},<,!>!!}}!iuo!>>}},{<!!!>!!!>!>!>},<'!o,{!a!>},<',\"<!!!!oe'>}}}}}}"
