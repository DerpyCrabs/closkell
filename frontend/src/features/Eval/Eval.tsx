import React from 'react'
import { useQuery } from 'react-query'
import { useLocalStorage } from 'react-use'
import { CircularProgress, Fade } from '@material-ui/core'
import { Alert, AlertTitle } from '@material-ui/lab'
import CodeInput from '../../components/CodeInput'
import { LVZipper, LispError, Result } from '../../types'
import Step from './Step'
import Steps from './Steps'

export default function Eval() {
  const [code, setCode] = useLocalStorage(
    'eval-expression',
    '(- (+ 1 2) 3)'
  ) as [string, (s: string) => void, () => void]

  const { isLoading, isError, error, data } = useQuery<
    Array<Result<LispError, LVZipper>>,
    Error
  >(code, () =>
    fetch('http://localhost:8081/eval', {
      method: 'POST',
      body: code,
    }).then((res) => res.json())
  )

  const [selectedStep, setSelectedStep] = React.useState(0)

  return (
    <div style={{ display: 'flex', flexDirection: 'row', flexGrow: 1 }}>
      <CodeInput code={code} setCode={setCode} />
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
              <Steps
                steps={data}
                selected={selectedStep}
                setSelected={setSelectedStep}
              />
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
