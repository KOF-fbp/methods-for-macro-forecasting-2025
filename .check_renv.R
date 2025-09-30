cat('Checking renv status...\n')
status <- renv::status()

# renv::status() returns TRUE when everything is in sync
if (isTRUE(status[["synchronized"]])) {
  cat('✅ renv status: All dependencies are in sync\n')
} else {
  cat('❌ renv status: Dependencies are out of sync\n')
  print(status)
  quit(status = 1)
}

