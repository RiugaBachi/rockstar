{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Style (
  pageStyle
) where

import Prelude hiding (rem, span, all, div, (**))
import Data.Bifunctor
import Data.Text (Text)
import Control.Monad
import Clay hiding (gold, black, white, red, grey, crimson, orange)
import qualified Clay.Flexbox as F

-- * Colors

black, white, cream, offwhite :: Color
grey, lightGrey, lighterGrey, lightestGrey :: Color
red, biege, crimson, gold, orange :: Color
black = "#000000e0"
white = "#ffffffe0"
cream = "#d0d1d5"
offwhite = "#e8e8e8"
grey = "#282828ff"
lightGrey = "#383838"
lighterGrey = "#444"
lightestGrey = "#8b8b8b"
red = "#ff4136"
biege = "#a3969c"
crimson = "#d24d57"
gold = "#ffd700e0"
orange = "#dd8338"

-- * Fonts

primFont, altFont, latexFont, codeFont :: Text
primFont = "Fira Sans"
altFont = "serif"
latexFont = "serif"
codeFont = "JetBrainsMonoMedium Nerd Font Mono"

-- * Utilities

setA' :: Int -> Color -> Color
setA' alpha = setA (realToFrac alpha / 0xFF)

-- * CSS

pageStyle :: Css
pageStyle = do
  animationStyle
  containerStyle
  elementStyle
  syntaxHighlightStyle
  typographyStyle

animationStyle :: Css
animationStyle = do
  keyframesFromTo "fadeIn" (opacity 0) (opacity 1)
  keyframesFromTo "fadeOut" (opacity 1) (opacity 0)
  ".fadedOut" ? do
    animationDuration (sec 1)
    animationFillMode forwards
    animationName "fadeOut"
  ".fadedIn" ? do
    animationDuration (sec 1)
    animationFillMode forwards
    animationName "fadeIn"

containerStyle :: Css
containerStyle = do
  star ? do
    boxSizing borderBox
    after & boxSizing borderBox
    before & boxSizing borderBox
  html ? fullSize
  body ? do
    fontFamily [primFont] []
    color lightGrey
    backgroundImage $ url "/img/wallpaper.png"
    backgroundPosition $ positioned (pct 67) (pct 60)
    backgroundSize cover
    backgroundColor crimson
    backgroundAttachment attachFixed
  mapM_ (? display block)
    [ article, aside, details
    , figcaption, figure, footer
    , header, menu, nav, section, summary
    ]
  ".hidden" ? display none
  ".header" ? do
    height (pct 100)
    position relative
    width (pct 100)
    minHeight (px 300)
    textAlign $ alignSide sideLeft
    color white
  ".flex" ? do
    display flex
    F.flex 0 1 auto
    flexDirection row
    flexWrap F.wrap
    alignItems center
  ".full-width" ?
    width (pct 100)
  ".container" ? do
    zIndex 1
    display block
    marginLeft none
    maxWidth (px 550)
    marginRight none
    paddingLeft (px 15)
    paddingRight (px 15)
    backgroundColor $ setA 0.95 white
    color $ setA' 0x90 grey
    borderRadius (px 3) (px 3) (px 3) (px 3)
    boxShadow . pure . bsColor (setA 0.7 grey) $ 
      shadowWithSpread 0 0 (px 12) 0
  ".container-post" ? do
    color lightGrey
    maxWidth none
    fontSize (px 14)
    sym2 padding (px 15) (px 35) 
  ".row" ? do
    marginTop (px 15)
    marginBottom (px 15)
    textAlign center
    ".stylized" & do
      marginTop none
      marginBottom none
  ".controls" ? do
    display flex
    flexDirection row
    width (pct 100)
  ".controls-left" ? do
    display flex
    justifyContent center
    alignItems center
    width (pct 50)
  ".controls-right" ? do
    width (px 255) 
    height (px 195.5)
    overflow hidden
  ".spotify-player" ? do
    backgroundColor black
    boxShadow . pure . bsColor (setA' 0xBB grey) $
      shadowWithSpread 0 0 (px 12) 0
    border solid (px 3) gold
    transforms 
      [ scale 0.85 0.85
      , translate (pct (-9)) (pct (-9))
      ]
  ".wrapper" ? do
    maxWidth (px 850)
    sym2 margin (rem 4) auto
    background $ setA' 0x3F white
    color lightGrey
    padding (em 2) (em 2) (em 2) (em 2)
    borderRadius (px 3) (px 3) (px 3) (px 3)
    boxShadow . pure . bsColor (setA 0.3 grey) $
      shadowWithSpread 0 0 (px 10) 0
  ".wrapper-container" ? do
    width auto
    margin auto auto auto auto
  ".entry-meta" ? do
    display block
    textTransform uppercase
    fontSize (px 12)
    color $ lighten 60 lightGrey
    paddingLeft (px 0)
    a ? color (lighten 60 lightGrey)
    li ? do
      listStyleType none
      display inline
    ".tag" ? do
      display inlineBlock
      sym borderRadius (px 3)
      border solid (px 1) transparent
      borderColor $ setA 0.5 cream
      textDecoration none
      backgroundColor $ setA 0.95 white
      color lightGrey
      transitionDuration (sec 0.75)
      sym2 margin (px 2) (px 6)
      boxShadow . pure . bsColor (setA 0.25 grey) $ 
        shadowWithSpread 0 0 (px 8) 0
      span ? do
        float floatLeft
        sym2 padding (px 4.5) (px 7)
      hover & backgroundColor (darken 0.15 white)
      ".count" ? do
        boxShadow . pure . bsColor (setA 0.25 grey) $ 
          shadowWithSpread 0 0 (px 8) 0
        backgroundColor $ darken 0.05 grey
        color $ gold
        sym2 borderRadius (px 3) (px 3)
  ".entry-title" ** a ?
    textDecoration none
  ".entry-tags" ? do
    display block
    margin (rem 1) auto (rem 1.5 @-@ px 32) auto
    textAlign $ alignSide sideLeft
  ".social-share" ? do
    display block
    marginTop (rem 1)
    width (pct 30)
    float floatRight
    textAlign $ alignSide sideRight
  ".feature" ? do
    transitionDuration (sec 0.5)
    color white
    padding (em 10) (em 10) (em 10) (em 10)
    marginLeft (rem (-4))
    marginRight (rem (-4))
    marginTop (rem (-2))
    borderRadius (px 3) (px 3) none none
    backgroundColor $ setA 0.6 black
    backgroundPosition $ placed sideCenter sideCenter
  "#disqus_thread" ? do
    width (pct 75)
    maxWidth (px 850)
    margin (rem (-3)) auto (rem 4) auto
    background $ setA 1.0 white
    color lightGrey
    sym padding (em 2)
    sym borderRadius (px 3)
    boxShadow . pure . bsColor (setA 0.7 grey) $ 
      shadowWithSpread 0 0 (px 10) 0
    transitionDuration (sec 0.5)
  where
    fullSize = do
      margin none none none none
      padding none none none none
      width (pct 100)
      height (pct 100)
      fontSize (pct 100)

elementStyle :: Css
elementStyle = do
  ".btn" ? do
    transition "all" (sec 0.15) easeIn (sec 0)
    padding (px 10) (px 18) (px 10) (px 18)
    margin (px 4) auto (px 4) auto
    display inlineBlock
    border solid (px 1) auto
    borderColor grey
    borderRadius (px 3) (px 3) (px 3) (px 3)
    textDecoration none
    background $ setA 0.88 white
    boxShadow . pure . bsColor (setA' 0xBB grey) $
      shadowWithSpread 0 0 (px 10) 0
    color grey
    hover & do
      boxShadow . pure . bsColor (setA 0.8 grey) $
        shadowWithSpread 0 0 (px 12) 0
      color red
    backgroundColor white
    borderColor orange
    fontSize (px 14)
  ".btn-return" ? do
    sym borderRadius (px 2)
    boxShadow . pure . bsColor (setA 0.8 grey) $
      shadowWithSpread 0 0 (px 12) 0
    hover & do
      boxShadow . pure . bsColor (setA 0.85 grey) $
        shadowWithSpread 0 0 (px 13) 0
    backgroundColor $ setA 0.95 white
    sym2 padding (px 16) (px 15)
  ".btn-social" ? do
    color grey
    fontSize (px 23)
    padding (px 4) (px 4) (px 4) (px 4)
    margin (px 0) (px 8) (px 0) (px 8)
  ".stylized" ? ".btn" ? do
    margin (px 10) (px 10) (px 10) (px 10)
    fontSize (px 16)
    fontWeight $ weight 300
  hr ? do
    border none (px 0) transparent
    height (px 3)
    opacity 0.3
    margin (px 15) auto (px 15) auto
    backgroundImage $ linearGradient (straight sideRight) 
      [ ("#ffffff00", 0)
      , ("#000000ff", 50)
      , ("#ffffff00", 100)
      ]
    ".light" ? opacity 0.2
  img ? do
    display block
    maxWidth (pct 100)
    width auto
    height auto
    verticalAlign vAlignTop
    backgroundSize cover
    marginLeft auto
    marginRight auto
    border solid (px 0) white
  ".img-circle" ? do
    sym borderRadius (pct 50)
    boxShadow . pure . bsColor (setA 0.8 grey) $
      shadowWithSpread 0 0 (px 12) 0
    marginTop (px 20)
    border solid (px 3) $ setA 0.85 gold
    height (px 200)
    width (px 200)
  ".author-photo" ? do
    marginTop (px 12)
    width (px 128)
    height (px 128)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    border solid (px 2) gold
    backgroundColor black
  ".image-right" ? do
    float floatRight
    margin none auto auto (em 1)
  figure ? do
    margin none none none none
    padding (px 10) auto (px 10) auto
    img ? do
      marginBottom (px 10)
      borderRadius (px 4) (px 4) (px 4) (px 4)
    ".half" & do
      img ? do
        width (pct 50 @-@ px 4)
        float floatLeft
        margin auto (px 1) auto (px 1)
      figcaption ? clear clearLeft
    ".third" ? do
      img ? do 
        width (pct 33.3 @-@ px 6)
        float floatLeft
        margin auto (px 1) auto (px 1)
  ".group-container" ? do
    borderRadius (px 3) (px 3) (px 3) (px 3)
    borderColor grey
    border solid (px 1) auto
  ".group-arrow" ? do
    borderColor lightGrey
  ".reading-time" ? do
    textAlign center
    fontWeight $ weight 300
    marginBottom (px 10)
            
syntaxHighlightStyle :: Css
syntaxHighlightStyle = do
  pre ? ".highlight" ?
    padding (em 1) (em 1) (em 1) (em 1)
  div |> ".sourceCode" ? do
    boxShadow . pure . bsColor (setA' 0xAA black) $
      shadowWithSpread 0 0 (px 5) 0
  ".sourceCode" ? do
    backgroundColor "#121212"
    color offwhite
    fontFamily [codeFont] []
    fontSize (px 12)
    borderRadius (px 4) (px 4) (px 4) (px 4)
    pre ? do
      overflowX hidden
      position relative
      margin none none none none
      padding (em 1) (em 1) (em 1) (em 1)
    ".hll" ? backgroundColor monokaiDarkGrey
    ".co" ? color monokaiGrey
    ".err" ? color monokaiDarkRed
    ".ge" ? fontStyle italic
    ".gs" ? fontWeight bold
    forM_ redSels (? (color monokaiRed <> fontWeight bold))
    forM_ blueSels (? color monokaiBlue)
    forM_ whiteSels (? color monokaiWhite)
    forM_ goldSels (? color monokaiGold)
    forM_ greenSels (? color monokaiGreen)
    forM_ purpleSels (? color monokaiPurple)
  where
    monokaiDarkGrey = "#49483e"
    monokaiGrey = "#75715e"
    monokaiDarkRed = "#950050"
    monokaiRed = "#f92672"
    monokaiWhite = "#f8f8f2"
    monokaiPurple = "#ae81ff"
    monokaiBlue = "#66d9ef"
    monokaiGold = "#e6db74"
    monokaiGreen = "#a6e22e"

    redSels =
      [ ".ot", ".kw", ".op" ]
    blueSels =
      []
    whiteSels =
      []
    goldSels =
      [ ".dt" ]
    purpleSels =
      []
    greenSels =
      [ ".st" ]

typographyStyle :: Css
typographyStyle = do
  title ? do
    textAlign center
    fontWeight bold
  blockquote ? do
    fontFamily [altFont] []
    fontStyle italic
    borderLeft solid (px 6) lighterGrey
    paddingLeft (px 20)
    sym margin (px 0)
  a ? do
    transitionDuration (sec 0.4)
    color lighterGrey
    link & color lighterGrey
    visited & color lighterGrey
    hover & color red
    active & transitionDuration (sec 0.3) <> color red
  figcaption ? do
    paddingTop (px 10)
    fontSize (px 14)
    color lightestGrey
  pre <> code ? do
    fontFamily [codeFont] []
    overflowX auto
  table ? width (pct 100)
  (p <> li) ** (pre <> code) ? do
    fontSize (px 14)
    backgroundColor offwhite
    border solid (px 1) cream
    sym borderRadius (px 4)
    sym2 margin (px 0) (px 2)
    sym2 padding (px 0) (px 5)
  li ** ".highlight" ** (pre <> code) ? do
    backgroundColor transparent
    border solid (px 0) transparent
  ".math" ?  do
    fontSize (px 18)
    fontFamily [latexFont] []
  ".notice" ? do
    marginTop (em 1.5)
    sym2 padding (em 0.5) (em 1)
    textIndent $ indent (px 0)
    fontSize (px 14)
    backgroundColor grey
    border solid (px 1) cream
    sym borderRadius (px 3)
  dt ? fontWeight bolder
  ".stylized" ? do
    fontSize (rem 1)
    fontWeight $ weight 300
  ".site-title" ? do
    marginTop (px 22)
    marginBottom (px 12)
    fontSize (rem 1.32)
    fontWeight $ weight 300
  ".content" ? do
    fontSize (px 14)
    lineHeight (unitless 1.85)
    marginBottom (rem 0.925)
    textAlign justify
  ".btn-return-wrapper" ? do
    textAlign center
    fontWeight bold
    marginBottom (rem 3)
    marginTop (rem 4 @-@ px 48)
  ".post-header" ? do
    textAlign center
    fontWeight bold
    marginBottom (rem 2)
  ".half-line" ?
    lineHeight (unitless 0.3) 
  "h3.stylized" ** a ? do
    color grey
    textDecoration none
  ".gold-star-wrapper" ? do
    display flex
    justifyContent center
    alignItems center
    marginBottom (px 20)
    marginLeft (px 8)
    marginRight (px 50)
  ".gold-star" ? do
    color gold
    fontSize (rem 1.75)
  ".li-header" ? do
    display flex
    justifyContent spaceBetween
  ".li-title" ? do
    float floatLeft
    sym padding (px 0)
  ".li-date-wrapper" ? do
    display flex
    alignItems center
  ".li-date" ? do
    float floatRight
    textAlign $ alignSide sideRight
    sym2 margin (px 0) auto
  ".li-flexbox" ? do
    display flex
    flexDirection row
  ".post-list" ? do
    textAlign $ alignSide sideLeft
    ul ? do
      listStyle none inside none
      paddingLeft (px 0)
      backgroundColor transparent
      boxShadow . pure . bsColor (setA' 0xBB grey) $
        shadowWithSpread 0 0 (px 12) 0
    li ? do
      backgroundColor transparent
      sym2 padding (px 11) (px 16) 
      borderRadius (px 3) (px 3) (px 3) (px 3)
      textDecoration none
      background $ linearGradient (straight sideLeft) $
        fmap (second (other . value))  $
          [ (black, px 75)
          , (gold, px 75)
          , (gold, px 81)
          , (black, px 81)
          , (white, px 84)
          ]
      color grey
      a ? do
        transitionDuration (sec 0.2)
        fontSize (px 20)
        textDecoration none
        color grey
        fontWeight bolder
        hover & color red
      p ? do
        fontWeight $ weight 300
        fontSize (px 16)
        marginTop (px 4)
      ".btn" ? do
        fontSize (px 14)
        fontWeight bolder

