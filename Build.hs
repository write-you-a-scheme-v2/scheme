
import Prelude
import Development.Shake
    ( cmd,
      shakeArgs,
      shakeOptions,
      getDirectoryFiles,
      (%>),
      need,
      phony,
      putInfo,
      removeFilesAfter,
      want )
import Development.Shake.FilePath ( (-<.>), (</>), dropDirectory1 )

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["output/scheme.html"]
    want ["wiki"]

    phony "wiki" $ do
      files <- fmap ("docs/" </>) <$> getDirectoryFiles "docs" ["*.md"]
      let targets = ["output" </> (f -<.> "wiki") | f <- files]
      need targets

    phony "clean" $ do
      putInfo "Cleaning files in output"
      removeFilesAfter "output" ["//*"]

    "output/scheme.html" %> \out -> do
        need ["resources/page.tmpl"]
        files <- fmap ("docs/" </>) <$> getDirectoryFiles "docs" ["*.md"]
        need files
        cmd ("pandoc" :: String) files ["-o" :: String] out (["--template", "resources/page.tmpl"] :: [String])

    "output//*.wiki" %> \out -> do
        let src = dropDirectory1 $ out -<.> "md"
        cmd ("pandoc" :: String) src ("-o" :: String) out (["-t", "mediawiki"] :: [String])
