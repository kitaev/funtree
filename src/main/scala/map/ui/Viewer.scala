package map.ui

import org.eclipse.swt.events.SelectionAdapter
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridLayout
import org.eclipse.jface.layout.GridDataFactory
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.graphics.GC
import org.eclipse.jface.dialogs.Dialog
import org.eclipse.swt.layout.FillLayout

object Viewer {
  def main(args: Array[String]) = {
    val display = new Display();
    val shell = new Shell(display);

    val t = new Viewer(shell, SWT.NONE)

    shell.setLayout(new FillLayout)
    shell.setText("RTree")
    shell.pack();
    shell.open();
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch()) display.sleep();
    }
    display.dispose();
  }

  def createFontMetrics(control: Control) = {
    val gc = new GC(control)
    try {
      gc.setFont(control.getFont())
      gc.getFontMetrics()
    } finally {
      gc.dispose()
    }
  }
  
  def gd(horizontalAlignment: Int = SWT.BEGINNING, grabHorizontal: Boolean = false,
    verticalAlignment: Int = SWT.CENTER, grabVertical: Boolean = false,
    minWidth: Int = 0, minHeight: Int = 0): GridData = {
    val gd = new GridData(horizontalAlignment, verticalAlignment, grabHorizontal, grabVertical)
    gd.minimumWidth = minWidth
    gd.minimumHeight = minHeight
    gd.widthHint = minWidth
    gd.heightHint = minHeight
    gd
  }
}

class Viewer(parent: Composite, style: Int) extends Composite(parent, style) {
  init()

  def init() = {
    val layout = new GridLayout(2, false)
    layout.horizontalSpacing = 0
    layout.verticalSpacing = 0
    setLayout(layout)

    val fm = Viewer.createFontMetrics(this)
    val rulerHeight = Dialog.convertVerticalDLUsToPixels(fm, 8)
    val rulerWidth = rulerHeight
    val display = parent.getDisplay()

    val square = new Composite(this, SWT.NONE)
    square.setLayoutData(Viewer.gd(SWT.BEGINNING, false, SWT.BEGINNING, false, rulerWidth, rulerHeight))
    square.setBackground(display.getSystemColor(SWT.COLOR_LIST_BACKGROUND))

    val topRuler = new Composite(this, SWT.NONE)
    topRuler.setLayoutData(Viewer.gd(SWT.FILL, true, SWT.BEGINNING, false, 0, rulerHeight))
    topRuler.setBackground(display.getSystemColor(SWT.COLOR_INFO_BACKGROUND))

    val leftRuler = new Composite(this, SWT.NONE)
    leftRuler.setLayoutData(Viewer.gd(SWT.BEGINNING, false, SWT.FILL, true, rulerWidth, 0))
    leftRuler.setBackground(display.getSystemColor(SWT.COLOR_INFO_BACKGROUND))

    val viewport = new Composite(this, SWT.NONE)
    val gd = Viewer.gd(SWT.FILL, true, SWT.FILL, true, 800, 600)
    gd.minimumHeight = 0
    gd.minimumWidth = 0
    viewport.setLayoutData(gd)
    viewport.setBackground(display.getSystemColor(SWT.COLOR_LIST_BACKGROUND))
  }
}