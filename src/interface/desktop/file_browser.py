# External Packages
from PyQt6 import QtWidgets
from PyQt6.QtCore import QDir

# Internal Packages
from src.utils.config import SearchType


class FileBrowser(QtWidgets.QWidget):
    def __init__(self, title, search_type: SearchType=None, default_files=[]):
        QtWidgets.QWidget.__init__(self)
        layout = QtWidgets.QHBoxLayout()
        self.setLayout(layout)
        self.search_type = search_type

        self.filter_name = self.getFileFilter(search_type)
        self.dirpath = QDir.homePath()

        self.label = QtWidgets.QLabel()
        self.label.setText(title)
        self.label.setFixedWidth(95)
        layout.addWidget(self.label)
        
        self.lineEdit = QtWidgets.QLineEdit(self)
        self.lineEdit.setFixedWidth(180)
        self.setFiles(default_files)
        
        layout.addWidget(self.lineEdit)
        
        self.button = QtWidgets.QPushButton('Add')
        self.button.clicked.connect(self.getFile)
        layout.addWidget(self.button)
        layout.addStretch()

    def setMode(self, search_type):
        self.search_type = search_type

    def getFileFilter(self, search_type):
        if search_type == SearchType.Org:
            return 'Org-Mode Files (*.org)'
        elif search_type == SearchType.Ledger:
            return 'Beancount Files (*.bean *.beancount)'
        elif search_type == SearchType.Markdown:
            return 'Markdown Files (*.md *.markdown)'
        elif search_type == SearchType.Music:
            return 'Org-Music Files (*.org)'
        elif search_type == SearchType.Image:
            return 'Images (*.jp[e]g)'

    def setDefaultDir(self, path):
        self.dirpath = path

    def getFile(self):
        filepaths = []
        if self.search_type == SearchType.Image:
            filepaths.append(QtWidgets.QFileDialog.getExistingDirectory(self, caption='Choose Directory',
                                                    directory=self.dirpath))
        else:
            filepaths.extend(QtWidgets.QFileDialog.getOpenFileNames(self, caption='Choose Files',
                                                    directory=self.dirpath,
                                                    filter=self.filter_name)[0])
        self.setFiles(filepaths)

    def setFiles(self, paths):
        self.filepaths = paths
        if not self.filepaths or len(self.filepaths) == 0:
            return
        elif len(self.filepaths) == 1:
            self.lineEdit.setText(self.filepaths[0])
        else:
            self.lineEdit.setText(",".join(self.filepaths))    

    def setLabelWidth(self, width):
        self.label.setFixedWidth(width)    

    def setlineEditWidth(self, width):
        self.lineEdit.setFixedWidth(width)

    def getPaths(self):
        return self.filepaths
