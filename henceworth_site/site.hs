--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        --compile $ pandocCompiler
        compile $ pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions extractNotes
            >>= loadAndApplyTemplate "templates/footnote.html" footnoteCtx
            >>= loadAndApplyTemplate "templates/default.html" footnoteCtx
            >>= relativizeUrls

        --compile $ pandocCompiler
        --    >>= loadAndApplyTemplate "templates/post.html"    postCtx
        --    >>= loadAndApplyTemplate "templates/default.html" postCtx
         --   >>= relativizeUrls

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


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList $ fmap (take 4) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

footnoteCtx :: Context String
footnoteCtx =
    constField "title" "footnote" `mappend`
    defaultContext

--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

extractNotes :: Pandoc -> Pandoc 
extractNotes (Pandoc meta blocks) = Pandoc meta blocks' 
    where blocks' = queryWith getNoteContents blocks 

getNoteContents :: Inline -> [Block] 
getNoteContents (Note bs) = bs 
getNoteContents _         = [] 
