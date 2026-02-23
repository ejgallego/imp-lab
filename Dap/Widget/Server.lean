/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.Widget.Types

open Lean Widget

namespace Dap

@[widget_module]
def traceExplorerWidget : Widget.Module where
  javascript := "
import * as React from 'react';
const e = React.createElement;

function clamp(v, lo, hi) {
  return Math.min(Math.max(v, lo), hi);
}

function groupProgramByFunction(program) {
  const groups = [];
  for (const line of program) {
    const last = groups.length > 0 ? groups[groups.length - 1] : null;
    if (!last || last.functionName !== line.functionName) {
      groups.push({ functionName: line.functionName, lines: [line] });
    } else {
      last.lines.push(line);
    }
  }
  return groups;
}

function renderCodeTokens(text, keyPrefix) {
  const tokens = text.match(/:=|[(),]|-?\\d+|[A-Za-z_][A-Za-z0-9_]*|\\s+|./g) || [];
  let prevWord = '';
  return tokens.map((token, i) => {
    if (/^\\s+$/.test(token)) {
      return token;
    }

    const style = {};
    if (token === 'let' || token === 'return' || token === 'call' || token === 'def') {
      style.color = '#0059b3';
      style.fontWeight = 650;
    } else if (token === 'add' || token === 'sub' || token === 'mul' || token === 'div') {
      style.color = '#b26a00';
      style.fontWeight = 600;
    } else if (/^-?\\d+$/.test(token)) {
      style.color = '#1b7f3b';
      style.fontWeight = 600;
    } else if (token === ':=' || token === ',' || token === '(' || token === ')') {
      style.color = '#6a7280';
    } else if (prevWord === 'call') {
      style.color = '#0f766e';
      style.fontWeight = 600;
    }

    if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(token)) {
      prevWord = token;
    }
    return e('span', { key: keyPrefix + '-tok-' + String(i), style }, token);
  });
}

export default function(props) {
  const states = props.states;
  const program = props.program;
  const programGroups = groupProgramByFunction(program);
  const maxIndex = Math.max(states.length - 1, 0);
  const [cursor, setCursor] = React.useState(0);
  const idx = clamp(cursor, 0, maxIndex);
  const state = states[idx];

  const canBack = idx > 0;
  const canForward = idx < maxIndex;

  const programSections = programGroups.map((group, groupIdx) =>
    e('div', { key: 'group-' + String(groupIdx), style: { marginBottom: '12px' } }, [
      e(
        'div',
        {
          key: 'header',
          style: {
            fontWeight: 700,
            borderBottom: '1px solid #e3e6ec',
            marginBottom: '4px',
            paddingBottom: '2px'
          }
        },
        [
          e(
            'span',
            {
              key: 'kw',
              style: { color: '#0059b3', fontWeight: 700 }
            },
            'def '
          ),
          e(
            'span',
            { key: 'name', style: { color: '#111827', fontWeight: 700 } },
            group.functionName
          ),
          e('span', { key: 'args', style: { color: '#6a7280' } }, '(...)')
        ]
      ),
      e(
        'ol',
        { key: 'lines', style: { margin: 0, paddingLeft: '20px' } },
        group.lines.map((line, lineIdx) =>
          e(
            'li',
            {
              key: 'line-' + String(lineIdx) + '-' + line.functionName + '-' + String(line.stmtLine),
              style: {
                background:
                  state.functionName === line.functionName && state.stmtLine === line.stmtLine
                    ? '#e9f2ff'
                    : 'transparent',
                borderRadius: '4px',
                padding: '2px 4px 2px 10px',
                marginBottom: '2px',
                whiteSpace: 'pre'
              }
            },
            [
              e(
                'span',
                { key: 'prefix', style: { color: '#6a7280' } },
                '[L' + String(line.sourceLine) + '] ' + String(line.stmtLine) + '  '
              ),
              ...renderCodeTokens(line.text, 'line-' + String(lineIdx))
            ]
          )
        )
      )
    ])
  );

  const callStackRows = state.callStack.map((frame, i) =>
    e(
      'li',
      {
        key: i,
        style: {
          fontWeight: i === 0 ? 700 : 400,
          opacity: i === 0 ? 1.0 : 0.85
        }
      },
      (i === 0 ? '-> ' : '   ') +
        frame.functionName +
        ':' +
        String(frame.stmtLine) +
        ' [L' +
        String(frame.sourceLine) +
        ']'
    )
  );

  const envRows = state.bindings.map((binding, i) =>
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
        e('span', { key: 'fn', style: { marginLeft: '8px' } }, 'fn = ' + String(state.functionName)),
        e('span', { key: 'pc', style: { marginLeft: '8px' } }, 'pc = ' + String(state.pc)),
        e('span', { key: 'stmt', style: { marginLeft: '8px' } }, 'stmt = ' + String(state.stmtLine)),
        e('span', { key: 'src', style: { marginLeft: '8px' } }, 'src = ' + String(state.sourceLine)),
        e('span', { key: 'depth', style: { marginLeft: '8px' } }, 'depth = ' + String(state.callDepth))
      ]),
      e('div', {
        key: 'body',
        style: { display: 'grid', gridTemplateColumns: '1.7fr 1fr', gap: '12px' }
      }, [
        e('div', { key: 'program' }, [
          e('div', { key: 'title', style: { marginBottom: '4px', fontWeight: 600 } }, 'Program'),
          e('div', { key: 'list' }, programSections)
        ]),
        e('div', { key: 'side' }, [
          e('div', { key: 'stack', style: { marginBottom: '12px' } }, [
            e('div', { key: 'title', style: { marginBottom: '4px', fontWeight: 600 } }, 'Call Stack'),
            callStackRows.length === 0
              ? e('p', { key: 'empty', style: { margin: 0, opacity: 0.7 } }, '(empty)')
              : e('ul', { key: 'rows', style: { margin: 0, paddingLeft: '20px' } }, callStackRows)
          ]),
          e('div', { key: 'env' }, [
            e('div', { key: 'title', style: { marginBottom: '4px', fontWeight: 600 } }, 'Environment'),
            envRows.length === 0
              ? e('p', { key: 'empty', style: { margin: 0, opacity: 0.7 } }, '(empty)')
              : e('ul', { key: 'rows', style: { margin: 0, paddingLeft: '20px' } }, envRows)
          ])
        ])
      ])
    ]);
}
"

end Dap
