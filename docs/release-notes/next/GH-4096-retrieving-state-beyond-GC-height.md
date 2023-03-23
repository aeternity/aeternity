* API requests that try to retrieve state from heights below the latest GC height will now receive a 410 ("gone")
  return code, instead of a 500 ("internal error"). **Potential incompatibility**: applications that depended on
  the past behavior will need to adapt.
