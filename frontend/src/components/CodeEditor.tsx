import 'ace-builds/src-noconflict/ace'
import 'ace-builds/src-min-noconflict/mode-clojure'
import 'ace-builds/src-min-noconflict/theme-gruvbox'
import 'ace-builds/src-min-noconflict/ext-language_tools'

import AceEditor from 'react-ace'

export default function CodeEditor({
  code,
  setCode,
}: {
  code: string
  setCode: (code: string) => void
}) {
  return (
    <AceEditor
      style={{
        backgroundColor: '#303030',
        resize: 'none',
        width: '100%',
        height: '100%',
      }}
      mode='clojure'
      fontSize={18}
      theme='gruvbox'
      onChange={setCode}
      value={code}
      wrapEnabled={true}
      tabSize={2}
      enableBasicAutocompletion={true}
    />
  )
}
