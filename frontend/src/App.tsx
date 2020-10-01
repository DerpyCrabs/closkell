import React from 'react'
import { QueryCache, ReactQueryCacheProvider, useQuery } from 'react-query'

const queryCache = new QueryCache()

function App() {
  return (
    <ReactQueryCacheProvider queryCache={queryCache}>
      <Steps />
    </ReactQueryCacheProvider>
  )
}

function Steps() {
  const { isLoading, error, data } = useQuery('eval', () =>
    fetch('http://localhost:8081/eval', {
      method: 'POST',
      // body: '(- (- 3 4) (+ (* 5 3) 5))',
      body: '(- (+ 1 2) 3)',
    }).then((res) => res.json())
  )
  const [selected, setSelected] = React.useState(0)
  if (isLoading) {
    return <div>"Loading..."</div>
  }
  if (error) {
    return <div>{JSON.stringify(error)}</div>
  }
  return (
    <div>
      <div style={{ display: 'flex', flexDirection: 'row' }}>
        {data.map((node: any, i: number) => (
          <button
            style={{ color: i === selected ? 'blue' : 'black' }}
            onClick={() => setSelected(i)}
          >
            Step {i}
          </button>
        ))}
      </div>
      {data.map(
        (node: any, i: number) =>
          i === selected && (
            <div>
              {node.Right !== undefined ? (
                <LispValView val={evalOutputTransform(node)} />
              ) : (
                JSON.stringify(node.Left)
              )}
            </div>
          )
      )}
    </div>
  )
}

const LispValView = ({ val }: any) => {
  if (val.type === 'list') {
    return (
      <span
        style={{ backgroundColor: val.focused ? 'lightblue' : 'unset' }}
        title={val.type}
      >
        {' '}
        (
        {val.value.map((v: any) => (
          <LispValView val={v} />
        ))}
        ){' '}
      </span>
    )
  } else {
    return (
      <span
        style={{ backgroundColor: val.focused ? 'lightblue' : 'unset' }}
        title={val.type}
      >
        {' '}
        {val.value}{' '}
      </span>
    )
  }
}

const evalOutputTransform = (step: any) => {
  if (step.Right !== undefined) {
    let val = {
      ...traverseLispVal((cr: any) => ({ ...cr, focused: false, env: [] }))(
        step.Right[1]
      ),
      env: step.Right[0],
      focused: true,
    }
    step.Right[2].forEach((crumb: any) => {
      val = {
        focused: false,
        env: crumb.env,
        type: 'list',
        value: [
          ...crumb.ls.map(
            traverseLispVal((cr: any) => ({ ...cr, focused: false, env: [] }))
          ),
          val,
          ...crumb.rs.map(
            traverseLispVal((cr: any) => ({ ...cr, focused: false, env: [] }))
          ),
        ],
      }
    })
    return val
  } else {
    return step
  }
}

const traverseLispVal = (f: any) => (val: any) => {
  if (val.type === 'list') {
    return { ...f(val), value: val.value.map(traverseLispVal(f)).map(f) }
  } else {
    return f(val)
  }
}
export default App
