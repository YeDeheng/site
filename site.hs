--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "resume.pdf" $ do
        route   idRoute
        compile copyFileCompiler

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "report.tex" $ do
        route $ setExtension "html"
        compile pandocCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.hs" $ do
        route     $ setExtension "css"
        compile   $ getResourceString
                >>= withItemBody (unixFilter "runghc" [])
                >>= return . fmap compressCss

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            let sitemapCtx =
                    field "posts" (\_ ->
                        genericPostList "templates/sitemap-item.xml" "%Y-%m-%d"
                                        recentFirst)
                    `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList $ fmap (take 5) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
genericPostCtx :: String -> Context String
genericPostCtx dateFormat =
    dateField "date" dateFormat `mappend`
    defaultContext


--------------------------------------------------------------------------------
genericPostList :: Identifier -> String -> ([Item String] -> Compiler [Item String]) -> Compiler String
genericPostList templatePath dateFormat sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody templatePath
    list    <- applyTemplateList itemTpl (genericPostCtx dateFormat) posts
    return list


--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList = genericPostList "templates/post-item.html" "%B %e, %Y"


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = genericPostCtx "%B %e, %Y"
