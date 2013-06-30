--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Data.Maybe (fromMaybe)
import           Data.Map as M
import           Text.Pandoc

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat)
import System.Locale (iso8601DateFormat)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (Hakyll.fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
        

    match "posts/*" $ version "toc" $
       compile $ pandocCompilerWithTransform defaultHakyllReaderOptions
                                    defaultHakyllWriterOptions {
                                        writerTableOfContents = True
                                      , writerTemplate = unlines 
                                                ["$toc$"
                                                ,"$footnotes$"]
                                   
                                      , writerStandalone = True
                                      } extractNotes

        

    -- Render posts list
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            itemTpl <- loadBody "templates/archive-item.html"
            list <- applyTemplateList itemTpl postCtx posts
            let archiveCtx = constField "title" "All posts" `mappend`
                             defaultContext
            makeItem list
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls


    match "index.html" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" (
                        mconcat [ constField "title" title
                                , constField "body" list
                                , constField "posts" "hello posts 2"
                                , defaultContext
                                ])
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls    


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx = mconcat [ dateField "date.machine" (iso8601DateFormat Nothing)
                  , dateField "date" "%B %e, %Y"
                  , field "toc" $ \item ->
                        loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
                  , modificationTimeField "updated.machine" (iso8601DateFormat Nothing)
                  , modificationTimeField "updated" "%B %e, %Y"
                  , dateField "date.day" "%d"
                  , dateField "date.month" "%b"
                  , dateField "date.year" "%Y"
                  , constField "posts" "hello posts"
                  , defaultContext
                  ]

feedCtx :: Context String
feedCtx = mconcat [ postCtx
                  , metadataField
                  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat [ tagsField "prettytags" tags
                       , postCtx
                       ]

sitemapCtx :: FeedConfiguration -> Context String
sitemapCtx conf = mconcat [ constField "root" (feedRoot conf)
                          , feedCtx
                          ]


footnoteCtx :: Context String
footnoteCtx =
  --  constField "title" "Oh lala" `mappend`
    titleContext `mappend`
  --  constField "footnotes" "myfootnote" `mappend`
    constField "body" "hello body"  `mappend`
    footContext  `mappend`
 --   defaultContext
    bodyField  "footnote" 



bodyFootNoteCtx :: Context String
bodyFootNoteCtx = mconcat
    [ footnoteCtx
    , field "body" $ return . itemBody
    ]

titleContext :: Context a
titleContext = field "title" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "No title" $ M.lookup "title" metadata    

footContext :: Context a
footContext = field "footnotes" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "No footnotes\n dasdkasdouasd \n asdouhasdouh" $ M.lookup "note" metadata    


--------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/archive-item.html"
    posts <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList postItemTpl (tagsCtx tags) posts

-- extractNotes, with the help of John MacFarlane
extractNotes :: Pandoc -> Pandoc 
extractNotes (Pandoc meta blocks) = Pandoc meta blocks' 
    where blocks' = queryWith getNoteContents blocks 

getNoteContents :: Inline -> [Block] 
getNoteContents (Note bs) = bs 
getNoteContents _         = [] 
