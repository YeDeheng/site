{-# LANGUAGE OverloadedStrings #-}
import Clay ( 

            (#)
            , (**)
            , (?)
            , Color
            , a
            , auto
            , black
            , body
            , bold
            , borderBottom
            , borderTop
            , code
            , color
            , div
            , end
            , float
            , floatLeft
            , fontFamily
            , fontSize
            , fontSize
            , fontStyle
            , fontWeight
            , h1
            , h2
            , italic
            , margin
            , marginBottom
            , marginLeft
            , marginTop
            , monospace
            , none
            , padding
            , parse
            , pre
            , putCss
            , px
            , solid
            , span
            , table
            , textAlign
            , textDecoration
            , textTransform
            , uppercase
            , width
            , html
            , backgroundColor
            , h3
            , h4
            , h5
            , h6
            , borderColor

            )
import Data.Monoid ((<>))

{-
 - Solarized color scheme
-}
sBase03, sBase02, sBase01, sBase00, sBase0, sBase1, sBase2, sBase3,
    sYellow, sOrange, sRed, sMagenta, sViolet, sBlue, sCyan, sGreen :: Color
sBase03     = parse "#002b36"
sBase02     = parse "#073642"
sBase01     = parse "#586e75"
sBase00     = parse "#657b83"
sBase0      = parse "#839496"
sBase1      = parse "#93a1a1"
sBase2      = parse "#eee8d5"
sBase3      = parse "#fdf6e3"
sYellow     = parse "#b58900"
sOrange     = parse "#cb4b16"
sRed        = parse "#dc322f"
sMagenta    = parse "#d33682"
sViolet     = parse "#6c71c4"
sBlue       = parse "#268bd2"
sCyan       = parse "#2aa198"
sGreen      = parse "#859900"

main :: IO ()
main = putCss $
    do html ?
         do backgroundColor sBase3
            color           sBase00
            "*" ?
              do color          sBase00
            h1 <> h2 <> h3 <> h4 <> h5 <> h6 ?
              do color          sBase01
                 borderColor    sBase00
            a <> a # ":active" ?
              do color          sBlue
            a # ":visited" ?
              do color          sBase01
       body ?
         do fontSize        (px 16)
            margin          (px 0) auto (px 0) auto
            width           (px 600)
       Clay.div # "#header" ?
         do borderBottom    solid (px 2) black
            marginBottom    (px 30)
            padding         (px 12) (px 0) (px 12) (px 0)
       Clay.div # "#logo" Clay.** a ?
         do float           floatLeft
            fontSize        (px 18)
            fontWeight      bold
            textDecoration  none
       Clay.div # "#header" Clay.** "#navigation" ?
         do textAlign       end
       Clay.div # "#header" Clay.** "#navigation" Clay.** a ?
         do fontSize        (px 18)
            fontWeight      bold
            marginLeft      (px 12)
            textDecoration  none
            textTransform   uppercase
       Clay.div # "#footer" ?
         do borderTop       solid (px 2) black
            color           sBase1
            fontSize        (px 12)
            marginTop       (px 30)
            padding         (px 12) (px 0) (px 12) (px 0)
            textAlign       end
       h1 ?
         do fontSize        (px 24)
       h2 ?
         do fontSize        (px 20)
       Clay.div # ".info" ?
         do color           sBase1
            fontSize        (px 14)
            fontStyle       italic
       pre # ".sourceCode" <> table # ".sourceCode" <> table # ".sourceCode" Clay.** "*" ?
         do fontFamily      [] [monospace]
       code # ".sourceCode" Clay.** Clay.span # ".kw" ?
         do color           sYellow
            fontWeight      bold
       code # ".sourceCode" Clay.** Clay.span # ".dt" ?
         do color           sCyan
       code # ".sourceCode" Clay.** Clay.span # ".dv" ?
         do color           sBase01
       code # ".sourceCode" Clay.** Clay.span # ".bn" ?
         do color           sOrange
       code # ".sourceCode" Clay.** Clay.span # ".fl" ?
         do color           sCyan
       code # ".sourceCode" Clay.** Clay.span # ".ch" ?
         do color           sRed
       code # ".sourceCode" Clay.** Clay.span # ".st" ?
         do color           sMagenta
       code # ".sourceCode" Clay.** Clay.span # ".co" ?
         do color           sBase01
            fontStyle       italic
       -- code # ".sourceCode" Clay.** Clay.span # ".ot" ? do { }
       code # ".sourceCode" Clay.** Clay.span # ".al" ?
         do color           sGreen
            fontWeight      bold
       code # ".sourceCode" Clay.** Clay.span # ".fu" ?
         do color           sBlue
       -- code # ".sourceCode" Clay.** Clay.span # ".re" ? do { }
       code # ".sourceCode" Clay.** Clay.span # ".er" ?
         do color           sRed
            fontWeight      bold
