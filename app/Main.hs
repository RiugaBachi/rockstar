module Main where

import Prelude hiding ((.), id)
import Data.List (intersperse, sort, sortOn, group)
import qualified Data.List as L
import Data.String (IsString)
import Data.Maybe
import Data.Bool
import Data.Char
import qualified Clay as C
import Control.Monad
import Control.Category
import Control.Arrow
import Data.Aeson
import Text.Pandoc
import Text.Pandoc.Highlighting
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text (Text)
import Development.Shake
import Lucid
import Lucid.Base
import Main.Utf8
import Rib (IsRoute)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc
import System.FilePath
import Style (pageStyle)

-- * Site Config

avatar :: IsString a => a
avatar = "img/avatar-kanami-1.png"

author :: IsString a => a
author = "Riuga"

wpm :: Int
wpm = 90

disqusShortname :: IsString a => a
disqusShortname = "riugabachi"

siteTitle :: IsString a => a
siteTitle = "Riuga's Programming Livehouse"

siteBio :: IsString a => [a]
siteBio = 
  [ "The deepest solace lies in understanding"
  , "This ancient unseen stream"
  , "A shudder before the beautiful" 
  ]

socials :: [(Text, Text)]
socials =
  [ ("https://github.com/riugabachi", "fab fa-fw fa-github")
  , ("https://reddit.com/u/monadic_riuga", "fab fa-fw fa-reddit")
  , ("https://twitch.tv/riugabachi", "fab fa-fw fa-twitch")
  , ("https://liberapay.com/riuga", "fas fa-fw fa-coins")
  , ("https://ko-fi.com/riuga", "fas fa-fw fa-donate")
  ]

spotifyUrl :: Text
spotifyUrl = "https://open.spotify.com/embed/playlist/5aXgrdQZWqTagrafhWf4kB?utm_source=generator&theme=0"

-- * Routes

data Route a where
  HomeR :: Route ()
  AboutR :: Route Pandoc
  PostListR :: Route [(Route Pandoc, Pandoc)]
  TaggedPostListR :: Text -> Route [(Route Pandoc, Pandoc)]
  PostR :: FilePath -> Route Pandoc
  NotFoundR :: Route Pandoc

instance IsRoute Route where
  routeFile HomeR = pure "index.html"
  routeFile AboutR = pure "about.html"
  routeFile PostListR = pure "posts.html"
  routeFile (TaggedPostListR tag) = pure $ "tag/" <> T.unpack tag <> ".html"
  routeFile (PostR srcPath) = pure $ srcPath -<.> ".html"
  routeFile NotFoundR = pure "404.html"

-- * Meta

data PostMeta
  = PostMeta 
      { postTitle :: Text
      , postDate :: Maybe Text
      , postDescription :: Maybe Text
      , postTags :: Maybe [Text]
      }
  deriving (Show, Eq)

Aeson.deriveFromJSON 
  (Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap toLower . drop 4 })
  ''PostMeta

getPostMeta :: Pandoc -> PostMeta
getPostMeta src = case Pandoc.extractMeta src of
  Nothing -> error "No YAML metadata"
  Just (Left e) -> error $ T.unpack e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v

-- * Generator

main :: IO ()
main = withUtf8 $ do
  Rib.run "content" "site" generateSite

allTags :: [Pandoc] -> [(Text, Int)]
allTags = 
  reverse
    . sortOn snd
    . uncurry zip
    . (fmap head &&& fmap length)
    . group 
    . sort
    . join 
    . fmap go
  where
    go =
      fmap (T.strip . T.toLower)
        . maybe [] id 
        . postTags 
        . getPostMeta

generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles ["img/**"]
  Rib.buildStaticFiles ["js/**"]
  -- Generate HTML for routes
  writeHtmlRoute HomeR ()
  writeHtmlRoute AboutR . head =<<
    Rib.forEvery ["about.md"] parseMarkdown
  posts <- reverse . catMaybes <$> Rib.forEvery ["posts/*.md"] \md ->
    if | "/_" `L.isInfixOf` md -> pure Nothing
       | otherwise      -> Just <$> parsePostMarkdown md
  let tags = fst <$> allTags (snd <$> posts)
  forM_ tags $ \t -> 
    writeHtmlRoute (TaggedPostListR t) $ 
      filter ((preprocessTag t `elem`) 
        . fmap preprocessTag 
        . maybe [] id 
        . postTags 
        . getPostMeta 
        . snd) posts
  writeHtmlRoute PostListR posts
  writeHtmlRoute NotFoundR . head =<<
    Rib.forEvery ["404.md"] parseMarkdown
  where
    writeHtmlRoute :: Route a -> a -> Action ()
    writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  
    preprocessTag = T.toLower . T.strip
    
    parseMarkdown =
      Pandoc.parse (const (Pandoc.readMarkdown mdOpts))

    parsePostMarkdown = 
      runKleisli ((PostR ^&& Kleisli parseMarkdown 
        >>> Kleisli (uncurry writeHtmlRoute) &&& id >>^ snd))

    mdOpts = def { readerExtensions = pandocExtensions }

    (^&&) = (&&&) . arr

renderPandoc :: Pandoc -> Html ()
renderPandoc = 
  toHtmlRaw
    . either (error "Failed to convert Pandoc to HTML5") id
    . runPure 
    . writeHtml5String opts 
  where
    opts = 
      def { writerExtensions = pandocExtensions
          , writerHTMLMathMethod = MathJax mempty
          , writerHighlightStyle = Just pygments
          }

renderPage :: forall a. Route a -> a -> Html ()
renderPage route val = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ $ toHtml routeTitle
    link_ 
      [ rel_ "icon"
      , href_ "/img/favicon.png" 
      ]
    link_ 
      [ rel_ "stylesheet"
      , href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.14.0/css/all.min.css"
      ]
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ [class_ "hidden"] $ renderRoute route
  scripts
  where
    routeTitle :: Text
    routeTitle = case route of
      HomeR -> siteTitle
      AboutR -> "About " <> author
      PostListR -> "Posts"
      TaggedPostListR tag -> "Posts tagged \"" <> tag <> "\""
      PostR _ -> postTitle (getPostMeta val)
      NotFoundR -> "404 Not Found"

    renderRoute :: Route a -> Html ()
    renderRoute HomeR =
      header_ [class_ "header flex", role_ "banner"] $
        div_ [class_ "wrapper wrapper-container flex"] . div_ [class_ "container"] $
          div_ [class_ "row"] $ do
            a_ [href_ $ Rib.routeUrl HomeR] $
              img_ [class_ "img-circle", src_ $ Rib.routeUrl HomeR <> avatar]
            h3_ [class_ "stylized"] $
              a_ [href_ $ Rib.routeUrl HomeR] $
                p_ [class_ "site-title"] siteTitle
            socialLinks
            hr_ []
            h3_ [class_ "stylized"] $
              a_ [href_ $ Rib.routeUrl HomeR] $
                p_ [] . sequence_ . intersperse (br_ mempty) $ siteBio
            hr_ []
            div_ [class_ "controls"] $ do
              div_ [class_ "controls-left"] $
                h3_ [class_ "stylized"] $
                  forM_ subroutes $ \x -> 
                    a_ [ class_ "btn"
                       , href_ $ Rib.routeUrl HomeR <> T.toLower x <> ".html"
                       ] (toHtml x)
              div_ [class_ "controls-right"] $
                iframe_
                  [ class_ "spotify-player"
                  , src_ spotifyUrl
                  , width_ "300"
                  , height_ "158"
                  , makeAttribute "loading" "lazy"
                  , makeAttribute "allow" "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture"
                  ] mempty
      where
        subroutes = ["About", "Posts", "Projects"]
    renderRoute AboutR =
      subpageLayout ("About " <> author) "fas fa-fw fa-home" HomeR mempty mempty $ do
        renderPandoc val
    renderRoute PostListR =
      subpageLayout "Posts" "fas fa-fw fa-home" HomeR tagsList mempty postList
      where
        tagsList =
          ul_ [class_ "entry-meta inline-list"] $
            forM_ (allTags $ snd <$> val) $ \(tag, count) ->
              li_ [] . a_ [class_ "tag", href_ $ "/tag/" <> tag <> ".html"] $ do
                span_ [] $ toHtml tag
                span_ [class_ "count"] $ toHtml $ show count
    renderRoute (TaggedPostListR _) =
      subpageLayout routeTitle "fas fa-fw fa-chevron-left" PostListR mempty mempty postList
    renderRoute (PostR _) = do
      let PostMeta{..} = getPostMeta val
      let content = renderPandoc val
      let icon = "fas fa-fw fa-chevron-left"
      flip (flip (subpageLayout postTitle icon PostListR) (tagSection postTags)) content $ do
        h4_ [] . toHtml $ maybe "Undated" id postDate
        p_ [class_ "reading-time"] $ do
          i_ [class_ "far fa-clock"] mempty
          toHtml $ " Reading time ~" <> (readingTime
            >>> show &&& (" minute" <>) . bool mempty "s" . (> 1)
            >>> uncurry (<>)) content
      section_ [id_ "disqus_thread"] $ mempty 
      where
        tagSection :: Maybe [Text] -> Html ()
        tagSection Nothing = mempty
        tagSection (Just ts) =
          div_ [class_ "entry-meta"] $ do
            br_ [class_ "half-line"]
            hr_ [class_ "light"]
            span_ [class_ "entry-tags"] $
              forM_ ts $ \tag ->
                a_ [class_ "tag", href_ $ "/tag/" <> tag <> ".html"] $
                  span_ [] $ toHtml tag

        readingTime = 
          max 1 . (`div` wpm) . length . LT.words . Lucid.renderText
    renderRoute NotFoundR =
      subpageLayout routeTitle "fas fa-fw fa-home" HomeR mempty mempty $
        renderPandoc val

    subpageLayout 
      :: Text -> Text -> Route x 
      -> Html () -> Html () -> Html () 
      -> Html ()
    subpageLayout pageTitle retIcon rt header footer contents =
      div_ [class_ "wrapper"] $ do
        div_ [class_ "content"] $ do
          div_ [class_ "btn-return-wrapper"] $
            a_ [class_ "btn btn-return", href_ $ Rib.routeUrl rt ] $
              i_ [class_ retIcon] mempty
          div_ [class_ "container container-post"] $ do
            div_ [class_ "post-header"] $ do
              h1_ [] $ toHtml pageTitle
              header
            hr_ [] <> br_ [class_ "half-line"]
            contents
        footer

    postList :: a ~ [(Route Pandoc, Pandoc)] => Html ()
    postList = do
      br_ [class_ "half-line"]
      div_ [class_ "post-list"] $
        forM_ val $ \(rt, post) -> do
          let PostMeta{..} = getPostMeta post
          ul_ [] . li_ [] . div_ [class_ "li-flexbox"] $ do
            div_ [class_ "gold-star-wrapper"] $
              i_ [class_ "fa fa-star gold-star"] mempty
            div_ [class_ "full-width"] $ do
              div_ [class_ "li-header"] $ do
                div_ [class_ "li-title"] $
                  a_ [href_ $ Rib.routeUrl rt] $ toHtml postTitle
                div_ [class_ "li-date-wrapper"] $
                  span_ [class_ "li-date"] . toHtml $ maybe "Undated" id postDate
              p_ [] . toHtml $ maybe "No description" id postDescription
              a_ [class_ "btn", href_ $ Rib.routeUrl rt] "Read More"

    socialLinks :: Html ()
    socialLinks =
      mapM_ (uncurry socialButton) socials
      where
        socialButton :: Text -> Text -> Html ()
        socialButton url icon =
          a_ [ class_ "btn btn-social"
             , href_ url
             , target_ "_blank"
             , rel_ "noopener noreferrer"
             ] $ i_ [class_ icon] mempty

    scripts :: Html ()
    scripts = do
      script_ [id_ "MathJax-script", async_ mempty, src_ "//cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js"] (mempty :: Html ())
      forM_ scriptSources $ \src -> script_ [src_ src] (mempty :: Html ())
      where
        scriptSources =
          [ "//cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js"
          , "/js/scripts.js"
          , "//" <> disqusShortname <> ".disqus.com/embed.js"
          ]
