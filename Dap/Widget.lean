import Lean
import Dap.Trace

open Lean Widget

namespace Dap

structure BindingView where
  name : String
  value : Int
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure StateView where
  pc : Nat
  bindings : Array BindingView
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure TraceWidgetProps where
  program : Array String
  states : Array StateView
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

def BindingView.ofPair (entry : Var × Value) : BindingView :=
  { name := entry.1, value := entry.2 }

def StateView.ofContext (ctx : Context) : StateView :=
  { pc := ctx.pc
    bindings := ctx.bindings.map BindingView.ofPair }

def TraceWidgetProps.ofTrace (trace : ExecutionTrace) : TraceWidgetProps :=
  { program := trace.program.render
    states := trace.states.map StateView.ofContext }

def traceWidgetProps (program : Program) : Except EvalError TraceWidgetProps := do
  let trace ← ExecutionTrace.build program
  pure (TraceWidgetProps.ofTrace trace)

@[widget_module]
def traceExplorerWidget : Widget.Module where
  javascript := "
import * as React from 'react';
const e = React.createElement;

function clamp(v, lo, hi) {
  return Math.min(Math.max(v, lo), hi);
}

export default function(props) {
  const states = Array.isArray(props.states) ? props.states : [];
  const program = Array.isArray(props.program) ? props.program : [];
  const maxIndex = Math.max(states.length - 1, 0);
  const [cursor, setCursor] = React.useState(0);
  const idx = clamp(cursor, 0, maxIndex);
  const state = states[idx] || { pc: 0, bindings: [] };

  const canBack = idx > 0;
  const canForward = idx < maxIndex;

  const programItems = program.map((line, i) =>
    e(
      'li',
      {
        key: i,
        style: {
          background: state.pc === i ? '#e9f2ff' : 'transparent',
          borderRadius: '4px',
          padding: '2px 4px'
        }
      },
      line
    )
  );

  const envRows = (state.bindings || []).map((binding, i) =>
    e('li', { key: i }, binding.name + ' = ' + String(binding.value))
  );

  return e('div', {
      style: {
        border: '1px solid #d5d8de',
        borderRadius: '6px',
        padding: '10px',
        fontFamily: 'ui-monospace, SFMono-Regular, Menlo, monospace'
      }
    }, [
      e('div', {
        key: 'controls',
        style: { display: 'flex', gap: '8px', alignItems: 'center', marginBottom: '8px' }
      }, [
        e('button', { key: 'back', onClick: () => setCursor(idx - 1), disabled: !canBack }, 'Back'),
        e('button', { key: 'forward', onClick: () => setCursor(idx + 1), disabled: !canForward }, 'Forward'),
        e('span', { key: 'step' }, 'State ' + String(idx) + '/' + String(maxIndex)),
        e('span', { key: 'pc', style: { marginLeft: '8px' } }, 'pc = ' + String(state.pc))
      ]),
      e('div', {
        key: 'body',
        style: { display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '12px' }
      }, [
        e('div', { key: 'program' }, [
          e('div', { key: 'title', style: { marginBottom: '4px', fontWeight: 600 } }, 'Program'),
          e('ol', { key: 'list', style: { margin: 0, paddingLeft: '20px' } }, programItems)
        ]),
        e('div', { key: 'env' }, [
          e('div', { key: 'title', style: { marginBottom: '4px', fontWeight: 600 } }, 'Environment'),
          envRows.length === 0
            ? e('p', { key: 'empty', style: { margin: 0, opacity: 0.7 } }, '(empty)')
            : e('ul', { key: 'rows', style: { margin: 0, paddingLeft: '20px' } }, envRows)
        ])
      ])
    ]);
}
"

end Dap
