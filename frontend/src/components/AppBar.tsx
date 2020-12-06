import React from 'react'
import { AppBar, IconButton, Toolbar, Typography } from '@material-ui/core'
import { Theme, createStyles, makeStyles } from '@material-ui/core/styles'
import MenuIcon from '@material-ui/icons/Menu'

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    menuButton: {
      marginRight: theme.spacing(2),
    },
    title: {
      flexGrow: 1,
    },
  })
)

export default function CloskellAppBar() {
  const classes = useStyles()
  return (
    <AppBar position='static' style={{ height: '64px' }}>
      <Toolbar>
        <IconButton
          edge='start'
          className={classes.menuButton}
          color='inherit'
          aria-label='menu'
        >
          <MenuIcon />
        </IconButton>
        <Typography variant='h6' className={classes.title}>
          Closkell
        </Typography>
      </Toolbar>
    </AppBar>
  )
}
