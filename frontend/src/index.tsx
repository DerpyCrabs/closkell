import 'fontsource-roboto'
import 'fontsource-roboto-mono'

import React from 'react'
import ReactDOM from 'react-dom'
import { QueryCache, ReactQueryCacheProvider } from 'react-query'
import CssBaseline from '@material-ui/core/CssBaseline'
import { ThemeProvider, createMuiTheme } from '@material-ui/core/styles'
import App from './App'

const queryCache = new QueryCache()

const theme = createMuiTheme({
  palette: {
    type: 'dark',
  },
})

ReactDOM.render(
  <React.StrictMode>
    <ReactQueryCacheProvider queryCache={queryCache}>
      <ThemeProvider theme={theme}>
        <CssBaseline />
        <App />
      </ThemeProvider>
    </ReactQueryCacheProvider>
  </React.StrictMode>,
  document.getElementById('root')
)
