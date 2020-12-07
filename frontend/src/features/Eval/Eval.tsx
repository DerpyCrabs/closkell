import React from 'react'
import { useQuery } from 'react-query'
import { useLocalStorage } from 'react-use'
import { CircularProgress, Divider, Fade } from '@material-ui/core'
import { Alert, AlertTitle } from '@material-ui/lab'
import { EvalRequest, LVZipper, LispError, Result } from '../../types'
import CodeInput from './RequestInput'
import Step from './Step'
import Steps from './Steps'

export default function Eval() {
  const [request, setRequest] = useLocalStorage('eval-expression', {
    typeCheck: true,
    macroExpand: true,
    expression: '(- (+ 1 2) 3)',
  }) as [EvalRequest, (s: EvalRequest) => void, () => void]

  const { isLoading, isError, error, data } = useQuery<
    Array<Result<LispError, LVZipper>>,
    Error
  >(JSON.stringify(request), () =>
    fetch('http://localhost:8081/eval', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(request),
    }).then((res) => res.json())
  )

  React.useEffect(
    () => setSelectedStep(data !== undefined ? data.length - 1 : 0),
    [data]
  )

  const [selectedStep, setSelectedStep] = React.useState(0)

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        height: 'calc(100vh - 64px)',
      }}
    >
      <CodeInput request={request} setRequest={setRequest} />
      <Divider orientation='vertical' flexItem />
      <div
        style={{
          display: 'flex',
          flex: 1,
          justifyContent: 'center',
          alignItems: 'center',
        }}
      >
        {isLoading ? (
          <Fade
            in={true}
            style={{
              transitionDelay: true ? '300ms' : '0ms',
            }}
            unmountOnExit
          >
            <CircularProgress
              style={{ width: '120px', height: '120px', color: 'grey' }}
            />
          </Fade>
        ) : isError ? (
          <Alert severity='error'>
            <AlertTitle>Error loading results</AlertTitle>
            {error && error.message}
          </Alert>
        ) : (
          data !== undefined && (
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                flex: 1,
                height: '100%',
              }}
            >
              {data.length !== 1 && (
                <Steps
                  steps={data}
                  selected={selectedStep}
                  setSelected={setSelectedStep}
                />
              )}
              {data.map((step: Result<LispError, LVZipper>, i: number) => (
                <Step step={step} value={selectedStep} index={i} key={i} />
              ))}
            </div>
          )
        )}
      </div>
    </div>
  )
}
