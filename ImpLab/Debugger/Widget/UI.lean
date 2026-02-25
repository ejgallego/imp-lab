/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import ImpLab.Debugger.Widget.Types

open Lean Widget

namespace ImpLab

@[widget_module]
def traceExplorerWidget : Widget.Module where
  javascript := "
import * as React from 'react';
import { useRpcSession } from '@leanprover/infoview';
const e = React.createElement;

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

function stopReasonStyle(stopReason, terminated) {
  if (terminated) {
    return {
      label: 'terminated',
      fg: '#7f1d1d',
      bg: '#fff1f2',
      border: '#fecdd3'
    };
  }
  if (stopReason === 'breakpoint') {
    return {
      label: 'breakpoint',
      fg: '#0f4c81',
      bg: '#ecfeff',
      border: '#67e8f9'
    };
  }
  if (stopReason === 'entry') {
    return {
      label: 'entry',
      fg: '#1d4ed8',
      bg: '#eef2ff',
      border: '#c7d2fe'
    };
  }
  if (stopReason === 'step') {
    return {
      label: 'step',
      fg: '#0e7490',
      bg: '#ecfeff',
      border: '#a5f3fc'
    };
  }
  if (stopReason === 'exception') {
    return {
      label: 'exception',
      fg: '#9a3412',
      bg: '#fff7ed',
      border: '#fed7aa'
    };
  }
  return {
    label: String(stopReason),
    fg: '#334155',
    bg: '#f8fafc',
    border: '#cbd5e1'
  };
}

function buttonStyle(variant, disabled) {
  if (disabled) {
    return {
      border: '1px solid #dbe4ee',
      background: 'linear-gradient(180deg, #f8fafc 0%, #eef2ff 100%)',
      color: '#94a3b8',
      borderRadius: '9px',
      padding: '6px 10px',
      fontSize: '12px',
      fontWeight: 700,
      letterSpacing: '0.03em'
    };
  }
  if (variant === 'continue') {
    return {
      border: '1px solid #0e7490',
      background: 'linear-gradient(180deg, #0891b2 0%, #0e7490 100%)',
      color: '#ffffff',
      borderRadius: '9px',
      padding: '6px 10px',
      fontSize: '12px',
      fontWeight: 700,
      letterSpacing: '0.03em',
      boxShadow: '0 0 0 1px rgba(255,255,255,0.2) inset, 0 6px 14px -9px rgba(14,116,144,0.8)'
    };
  }
  return {
    border: '1px solid #bae6fd',
    background: 'linear-gradient(180deg, #ffffff 0%, #f0f9ff 100%)',
    color: '#0f172a',
    borderRadius: '9px',
    padding: '6px 10px',
    fontSize: '12px',
    fontWeight: 700,
    letterSpacing: '0.03em',
    boxShadow: '0 1px 0 #ffffff inset'
  };
}

function panel(title, content, key) {
  return e(
    'section',
    {
      key,
      style: {
        border: '1px solid #dbeafe',
        background: 'linear-gradient(170deg, rgba(255,255,255,0.96) 0%, rgba(240,249,255,0.95) 100%)',
        borderRadius: '10px',
        padding: '8px 9px',
        boxShadow: '0 10px 25px -22px rgba(14,116,144,0.65)'
      }
    },
    [
      e(
        'div',
        {
          key: 'title',
          style: {
            marginBottom: '6px',
            fontWeight: 700,
            fontSize: '11px',
            color: '#0e7490',
            textTransform: 'uppercase',
            letterSpacing: '0.08em'
          }
        },
        title
      ),
      content
    ]
  );
}

function renderCodeTokens(text, keyPrefix) {
  const tokens = text.match(/:=|[(),]|-?\\d+|[A-Za-z_][A-Za-z0-9_]*|\\s+|./g) || [];
  let prevWord = '';
  return tokens.map((token, i) => {
    if (/^\\s+$/.test(token)) {
      return token;
    }

    const style = {};
    if (
      token === 'let' ||
      token === 'set' ||
      token === 'get' ||
      token === 'return' ||
      token === 'call' ||
      token === 'def'
    ) {
      style.color = '#0e7490';
      style.fontWeight = 700;
    } else if (token === 'add' || token === 'sub' || token === 'mul' || token === 'div') {
      style.color = '#b45309';
      style.fontWeight = 600;
    } else if (/^-?\\d+$/.test(token)) {
      style.color = '#166534';
      style.fontWeight = 650;
    } else if (token === ':=' || token === ',' || token === '(' || token === ')') {
      style.color = '#64748b';
    } else if (prevWord === 'call') {
      style.color = '#0f766e';
      style.fontWeight = 700;
    }

    if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(token)) {
      prevWord = token;
    }
    return e('span', { key: keyPrefix + '-tok-' + String(i), style }, token);
  });
}

export default function(props) {
  const rs = useRpcSession();
  const [session, setSession] = React.useState(null);
  const [busy, setBusy] = React.useState(false);
  const [error, setError] = React.useState(null);
  const [narrow, setNarrow] = React.useState(false);
  const sessionIdRef = React.useRef(null);
  const launchParams = {
    programInfo: props.programInfo,
    stopOnEntry: props.stopOnEntry ?? true,
    breakpoints: props.breakpoints ?? []
  };
  const launchSignature = JSON.stringify(launchParams);

  React.useEffect(() => {
    if (typeof window === 'undefined') return;
    const updateNarrow = () => setNarrow(window.innerWidth < 960);
    updateNarrow();
    window.addEventListener('resize', updateNarrow);
    return () => window.removeEventListener('resize', updateNarrow);
  }, []);

  React.useEffect(() => {
    let cancelled = false;
    async function launch() {
      setBusy(true);
      setError(null);
      try {
        const launched = await rs.call('ImpLab.Debugger.Widget.Server.widgetLaunch', launchParams);
        if (!cancelled) {
          sessionIdRef.current = launched.sessionId;
          setSession(launched);
        }
      } catch (e) {
        if (!cancelled) setError(String(e));
      } finally {
        if (!cancelled) setBusy(false);
      }
    }
    launch();
    return () => {
      cancelled = true;
      if (sessionIdRef.current !== null) {
        rs.call('ImpLab.Debugger.Widget.Server.widgetDisconnect', { sessionId: sessionIdRef.current }).catch(() => {});
        sessionIdRef.current = null;
      }
    };
  }, [rs, launchSignature]);

  async function control(method) {
    if (!session) return;
    setBusy(true);
    setError(null);
    try {
      const updated = await rs.call(method, { sessionId: session.sessionId });
      sessionIdRef.current = updated.sessionId;
      setSession(updated);
    } catch (e) {
      setError(String(e));
    } finally {
      setBusy(false);
    }
  }

  if (error) {
    return e('pre', {
      style: {
        color: '#b42318',
        margin: 0,
        border: '1px solid #fecaca',
        background: '#fef2f2',
        borderRadius: '8px',
        padding: '10px'
      }
    }, String(error));
  }
  if (!session) {
    return e('div', {
      style: {
        border: '1px solid #e2e8f0',
        borderRadius: '8px',
        padding: '10px',
        color: '#475569'
      }
    }, busy ? 'Launching debugger session...' : 'No session');
  }

  const state = session.state;
  const program = session.program;
  const programGroups = groupProgramByFunction(program);
  const statusTone = stopReasonStyle(session.stopReason, session.terminated);

  const programSections = programGroups.map((group, groupIdx) =>
    e('div', { key: 'group-' + String(groupIdx), style: { marginBottom: '12px' } }, [
      e(
        'div',
        {
          key: 'header',
          style: {
            fontWeight: 700,
            borderBottom: '1px solid #e2e8f0',
            marginBottom: '6px',
            paddingBottom: '4px'
          }
        },
        [
          e(
            'span',
            {
              key: 'kw',
              style: { color: '#0f4c81', fontWeight: 700 }
            },
            'def '
          ),
          e(
            'span',
            { key: 'name', style: { color: '#0f172a', fontWeight: 700 } },
            group.functionName
          ),
          e('span', { key: 'args', style: { color: '#64748b' } }, '(...)')
        ]
      ),
      e(
        'ol',
        { key: 'lines', style: { margin: 0, paddingLeft: '22px' } },
        group.lines.map((line, lineIdx) =>
          e(
            'li',
            {
              key: 'line-' + String(lineIdx) + '-' + line.functionName + '-' + String(line.stmtLine),
              style: {
                background:
                  state.functionName === line.functionName && state.stmtLine === line.stmtLine
                    ? '#cffafe'
                    : 'transparent',
                border: state.functionName === line.functionName && state.stmtLine === line.stmtLine
                  ? '1px solid #0ea5a4'
                  : '1px solid transparent',
                borderRadius: '6px',
                padding: '1px 6px 1px 10px',
                boxShadow:
                  state.functionName === line.functionName && state.stmtLine === line.stmtLine
                    ? 'inset 4px 0 0 #0f766e'
                    : 'none',
                marginBottom: '1px',
                whiteSpace: 'pre'
              }
            },
            [
              e(
                'span',
                { key: 'prefix', style: { color: '#64748b' } },
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
          fontWeight: i === 0 ? 700 : 500,
          opacity: i === 0 ? 1.0 : 0.78,
          marginBottom: '2px'
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

  const localRows = state.bindings.map((binding, i) =>
    e('li', { key: i, style: { marginBottom: '2px' } }, binding.name + ' = ' + String(binding.value))
  );
  const heapRows = state.heapBindings.map((binding, i) =>
    e('li', { key: i, style: { marginBottom: '2px' } }, binding.name + ' = ' + String(binding.value))
  );

  const metaPillStyle = {
    border: '1px solid #bae6fd',
    borderRadius: '999px',
    background: 'linear-gradient(180deg, #ffffff 0%, #f0f9ff 100%)',
    padding: '1px 6px',
    color: '#0f4c81',
    fontSize: '10px'
  };

  return e('div', {
      style: {
        border: '1px solid #cfe7ff',
        borderRadius: '12px',
        padding: '12px',
        background:
          'radial-gradient(120% 120% at 10% 0%, rgba(186,230,253,0.38) 0%, rgba(255,255,255,0) 45%), ' +
          'radial-gradient(120% 120% at 100% 100%, rgba(199,210,254,0.26) 0%, rgba(255,255,255,0) 40%), ' +
          'linear-gradient(180deg, #f8fbff 0%, #ffffff 100%)',
        fontFamily: 'IBM Plex Mono, ui-monospace, SFMono-Regular, Menlo, monospace',
        fontSize: '12px',
        lineHeight: 1.42,
        boxShadow: '0 20px 45px -36px rgba(14,116,144,0.75)'
      }
    }, [
      e('div', {
        key: 'hud',
        style: {
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center',
          marginBottom: '8px',
          paddingBottom: '6px',
          borderBottom: '1px solid #dbeafe',
          color: '#0f4c81',
          fontSize: '10px',
          letterSpacing: '0.08em',
          textTransform: 'uppercase'
        }
      }, [
        e('span', { key: 'left' }, 'ImpLab Trace Console'),
        e('span', { key: 'right', style: { opacity: 0.72 } }, 'Live Session')
      ]),
      e('div', {
        key: 'controls',
        style: {
          display: 'flex',
          gap: '8px',
          alignItems: 'center',
          marginBottom: '10px',
          flexWrap: 'wrap'
        }
      }, [
        e('button', {
          key: 'back',
          onClick: () => control('ImpLab.Debugger.Widget.Server.widgetStepBack'),
          disabled: busy,
          style: buttonStyle('default', busy)
        }, 'Step Back'),
        e('button', {
          key: 'forward',
          onClick: () => control('ImpLab.Debugger.Widget.Server.widgetStepIn'),
          disabled: busy,
          style: buttonStyle('default', busy)
        }, 'Step In'),
        e('button', {
          key: 'cont',
          onClick: () => control('ImpLab.Debugger.Widget.Server.widgetContinue'),
          disabled: busy,
          style: buttonStyle('continue', busy)
        }, 'Continue'),
        e('span', {
          key: 'status',
          style: {
            marginLeft: '4px',
            border: '1px solid ' + statusTone.border,
            borderRadius: '999px',
            background: statusTone.bg,
            color: statusTone.fg,
            padding: '3px 9px',
            fontWeight: 700,
            textTransform: 'uppercase',
            letterSpacing: '0.08em',
            fontSize: '10px',
            boxShadow: '0 0 0 1px rgba(255,255,255,0.8) inset'
          }
        }, statusTone.label)
      ]),
      e('div', {
        key: 'meta',
        style: {
          display: 'flex',
          gap: '4px',
          flexWrap: 'wrap',
          marginBottom: '8px'
        }
      }, [
        e('span', { key: 'fn', style: metaPillStyle }, 'f:' + String(state.functionName)),
        e('span', { key: 'pc', style: metaPillStyle }, 'pc:' + String(state.pc)),
        e('span', { key: 'stmt', style: metaPillStyle }, 'st:' + String(state.stmtLine)),
        e('span', { key: 'src', style: metaPillStyle }, 'L' + String(state.sourceLine)),
        e('span', { key: 'depth', style: metaPillStyle }, 'd:' + String(state.callDepth))
      ]),
      e('div', {
        key: 'body',
        style: {
          display: 'grid',
          gridTemplateColumns: narrow ? '1fr' : 'minmax(0, 1.7fr) minmax(250px, 1fr)',
          gap: '10px'
        }
      }, [
        panel('Program', e('div', { key: 'list' }, programSections), 'program'),
        e('div', { key: 'side', style: { display: 'grid', gap: '10px', alignContent: 'start' } }, [
          panel(
            'Call Stack',
            callStackRows.length === 0
              ? e('p', { key: 'empty', style: { margin: 0, color: '#64748b' } }, '(empty)')
              : e('ul', { key: 'rows', style: { margin: 0, paddingLeft: '18px' } }, callStackRows),
            'stack'
          ),
          e('div', {
            key: 'locals-heap',
            style: {
              display: 'grid',
              gap: '10px',
              gridTemplateColumns: 'repeat(auto-fit, minmax(180px, 1fr))'
            }
          }, [
            panel(
              'Locals',
              localRows.length === 0
                ? e('p', { key: 'empty', style: { margin: 0, color: '#64748b' } }, '(empty)')
                : e('ul', { key: 'rows', style: { margin: 0, paddingLeft: '18px' } }, localRows),
              'locals'
            ),
            panel(
              'Heap',
              heapRows.length === 0
                ? e('p', { key: 'empty', style: { margin: 0, color: '#64748b' } }, '(empty)')
                : e('ul', { key: 'rows', style: { margin: 0, paddingLeft: '18px' } }, heapRows),
              'heap'
            )
          ])
        ])
      ])
    ]);
}
"

end ImpLab
