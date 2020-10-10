import React from 'react'
import { useQuery } from 'react-query'
import CodeInput from '../../components/CodeInput'
import { LVZipper, LispError, Result } from '../../types'
import Step from './Step'
import Steps from './Steps'

export default function Eval() {
  const [code, setCode] = React.useState('(- (+ 1 2) 3)')
  const { isLoading, error, data } = useQuery(code, () =>
    fetch('http://localhost:8081/eval', {
      method: 'POST',
      body: code,
    }).then((res) => res.json())
  )

  const [selectedStep, setSelectedStep] = React.useState(0)

  if (isLoading) {
    return <div>"Loading..."</div>
  }
  if (error) {
    return <div>{JSON.stringify(error)}</div>
  }

  return (
    <div style={{ display: 'flex', flexDirection: 'row', flexGrow: 1 }}>
      <CodeInput code={code} setCode={setCode} />
      <Steps
        steps={data}
        selected={selectedStep}
        setSelected={setSelectedStep}
      />
      {data.map((step: Result<LispError, LVZipper>, i: number) => (
        <Step step={step} value={selectedStep} index={i} />
      ))}
    </div>
  )
}
