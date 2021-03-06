{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll
import           Text.Regex

projectsPattern :: Pattern
projectsPattern = "projects/*.md" .||. "projects/*.org"

postsPattern :: Pattern
postsPattern = "posts/*.md" .||. "posts/*.org"

main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  match (fromList ["resume.org"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/resume.html" defaultContext

  match postsPattern $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= saveSnapshot "post-content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match projectsPattern $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/project.html" defaultContext
      >>= saveSnapshot "proj-content"
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match images $ do
    version "raw" $ do
      route (gsubRoute "gallery/" (const "gallery/raw/"))
      compile copyFileCompiler
    version "thumb" $ do
      route (gsubRoute "gallery/" (const "gallery/thumb/")
            `composeRoutes` setExtension "jpg")
      compile $ do
        path <- getResourceFilePath
        makeItem =<< unixFilterLBS "bin/mkthumb" [path, "128x128"] mempty
    version "thumb@2x" $ do
      route (gsubRoute "gallery/" (const "gallery/thumb/")
            `composeRoutes` setExtension "jpg@2x")
      compile $ do
        path <- getResourceFilePath
        makeItem =<< unixFilterLBS "bin/mkthumb" [path, "256x256"] mempty
    version "html" $ do
      route (setExtension "html")
      compile $ getResourceFilePath
        >>= makeItem
        >>= loadAndApplyTemplate "templates/image.html"   galleryCtx
        >>= saveSnapshot "html_gal"
        >>= loadAndApplyTemplate "templates/gallery_wrapper.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      projs <- recentFirst =<< loadAll projectsPattern
      imgs  <- fmap (take 10) . recentFirst =<< loadAllSnapshots (images .&&.
                                                                  hasVersion "html") "html_gal"
      let indexCtx = mconcat
            [ listField "posts" postCtx (return posts)
            , listField "projects" defaultContext (return projs)
            , listField "gallery" galleryCtx (return imgs)
            , defaultContext
            ]
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      projs <- loadAllSnapshots projectsPattern "proj-content"
      posts <- loadAllSnapshots postsPattern "post-content"
      both  <- recentFirst (projs ++ posts)
      renderRss feedCfg feedCtx both

  create ["posts/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      let ctx = mconcat
            [ listField "posts" postCtx (return posts)
            , constField "title" "Posts"
            , defaultContext
            ]
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  create ["gallery/index.html"] $ do
    route idRoute
    compile $ do
      imgs  <- recentFirst =<< loadAllSnapshots (hasVersion "html") "html_gal"
      let ctx = mconcat
            [ listField "gallery" galleryCtx (return imgs)
            , constField "title" "Gallery"
            , defaultContext
            ]
      makeItem ""
        >>= loadAndApplyTemplate "templates/gallery.html" ctx
        >>= relativizeUrls

  create ["projects/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll projectsPattern
      let ctx = mconcat
            [ listField "projects" defaultContext (return posts)
            , constField "title" "Projects"
            , defaultContext
            ]
      makeItem ""
        >>= loadAndApplyTemplate "templates/project-list.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $
    compile templateCompiler

galleryCtx :: Context String
galleryCtx = postCtx <> galleryVersion "raw" <> galleryVersion "thumb"

galleryVersion :: String -> Context String
galleryVersion v =
  field v (return . setExt . replacer . toFilePath . itemIdentifier)
  <> field "description" (\item -> do
    meta <- getMetadataField (itemIdentifier item) "description"
    return (maybe "" id meta))
 where
  setExt exp =
    if v == "thumb"
    then case splitRegex (mkRegex "\\.") exp of
           p : _ -> p ++ ".jpg"
           _     -> exp
    else exp
  replacer r = case splitAll "/" r of
    "gallery":_ -> "/gallery/" ++ v ++ '/':drop 8 r
    _           -> r

images :: Pattern
images = fromRegex "gallery/.(.*)\\.(jpg|jpeg|png|bmp|gif|tiff|tif|webp|svg)$"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration
  { feedTitle       = "Quasimal"
  , feedDescription = "There are no musings here."
  , feedAuthorName  = "Mike Ledger"
  , feedAuthorEmail = "mike@quasimal.com"
  , feedRoot        = "http://quasimal.com"
  }
