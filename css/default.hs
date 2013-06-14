{-# LANGUAGE OverloadedStrings #-}
import Clay ( 

            (#)
            , (**)
            , (?)
            , Abs
            , Color
            , Size
            , a
            , auto
            , backgroundColor
            , body
            , bold
            , borderColor
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
            , h3
            , h4
            , h5
            , h6
            , html
            , italic
            , lineHeight
            , margin
            , marginBottom
            , marginLeft
            , marginTop
            , monospace
            , none
            , overflow
            , padding
            , parse
            , pct
            , pre
            , putCss
            , px
            , sansSerif
            , span
            , table
            , textAlign
            , textDecoration
            , width

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

pageWidth, vMargin :: Size Abs
pageWidth = px 960
vMargin = px 20

main :: IO ()
main = putCss $
    do html ?
         do backgroundColor sBase3
            color           sBase00
            "*" ?
              do color              sBase00
            h1 <> h2 <> h3 <> h4 <> h5 <> h6 ?
              do color              sBase01
                 borderColor        sBase00
                 fontWeight         bold
            a <> a # ":active" <> a # ":visited" ?
              do color              (parse "#607890")
            a # ":hover" ?
              do color              (parse "#036")
            h1 # "#pagetitle" <> h1 # ":first-of-type" ?
              do fontSize           (pct 300)
                 marginTop          0
                 marginBottom       vMargin
                 lineHeight         (pct 100)
            -- h2 # "#pagesubtitle" <> h2 # ":first-of-type" ?
            --   do fontSize           (pct 200)
            --      textTransform      none
            --      marginTop          vMargin
            --      -- marginBottom       (fmap (*2) vMargin)
            --      lineHeight         (pct 100)
            fontFamily          ["Helvetica", "Arial"] [sansSerif]
       body ?
         do fontSize        (px 16)
            margin          (px 0) auto (px 0) auto
       Clay.div # "#header-strip" ?
         do padding         (px 12) (px 0) (px 12) (px 0)
            backgroundColor sBlue
            overflow        auto
       Clay.div # "#header" ?
         do width           pageWidth
            margin          (px 0) auto (px 0) auto
       Clay.div # "#logo" ?
         do float           floatLeft
       Clay.div # "#header" Clay.** "#logo" Clay.** a ?
         do color           sBase2
            fontSize        (px 20)
            fontWeight      bold
            textDecoration  none
       Clay.div # "#logo" Clay.** Clay.span ?
         do color           sBase2
            fontSize        (px 15)
            fontStyle       italic
       Clay.div # "#header" Clay.** "#navigation" ?
         do textAlign       end
       Clay.div # "#header" Clay.** "#navigation" Clay.** a ?
         do color           sBase2
            fontSize        (px 18)
            fontWeight      bold
            marginLeft      (px 12)
            textDecoration  none
       Clay.div # "#container" ?
         do width           pageWidth
            margin          (px 0) auto (px 0) auto
            padding         (px 30) (px 0) (px 30) (px 0)
       Clay.div # "#content" ?
         do padding         (px 0) (px 10) (px 0) (px 10)
       Clay.div # "#footer-strip" ?
         do overflow        auto
            padding         (px 12) (px 0) (px 12) (px 0)
            -- backgroundColor sBlue
       Clay.div # "#footer" ?
         do color           sBase1
            fontSize        (px 12)
            textAlign       end
            width           pageWidth
            margin          (px 0) auto (px 0) auto
       h1 ?
         do fontSize        (px 24)
       h2 ?
         do fontSize        (px 20)
       Clay.div # ".info" ?
         do color           sBase1
            fontSize        (px 14)
            fontStyle       italic
       pre # ".sourceCode" <> table # ".sourceCode" <> table # ".sourceCode" Clay.** "*" ?
         do fontFamily      [ "Monaco", "Inconsolata", "DejaVu Sans Mono"
                            , "Courier New", "Courier"
                            ]
                            [monospace]
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
