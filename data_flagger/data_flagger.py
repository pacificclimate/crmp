# TODO:
# Add a set of selections
# add to the selection set on each click
# add a keypress or menu option to flag all currently selected observations and insert into database
# need to add PCIC flags to database

import sys, os
import datetime
from optparse import OptionParser
from pdb import set_trace

import psycopg2
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from matplotlib.widgets import  RectangleSelector
from matplotlib.backend_bases import PickEvent
import scipy
import numpy
import pylab

class MyFigure(Figure):
    def __init__(self, *args, **kwargs):
        self.selected = numpy.empty((3,0))
        self.unselected = None
        Figure.__init__(self, *args, **kwargs)

    def PostInit(self, ts):
        self.unselected = ts.data.transpose()
        ax = self.add_subplot(111)
        ax.set_title('%s data for station %s' % (ts.variable, ts.station_id))
        self.unselected_points, = ax.plot(self.unselected[0,:], self.unselected[1,:], 'bo', picker=5)  # 5 points tolerance
        self.selected_points, = ax.plot([], [], 'ro', zorder=5)
        self.canvas.mpl_connect('pick_event', self.OnPick)
        self.rs = RectangleSelector(ax, self.OnSelect, drawtype='box', button=3)
        self.canvas.mpl_connect('key_press_event', self.OnKeyPress)

    def OnSelect(self, eclick, erelease):
        'eclick and erelease are matplotlib events at press and release'
        print ' startposition : (%f, %f)' % (eclick.xdata, eclick.ydata)
        print ' endposition   : (%f, %f)' % (erelease.xdata, erelease.ydata)
        print ' used button   : ', eclick.button
        o = datetime.datetime(1, 1, 1)

        xdata = map(datetime.timedelta, (eclick.xdata, erelease.xdata))
        t0 = o + min(xdata)
        tn = o + max(xdata)
        ydata = (eclick.ydata, erelease.ydata)
        y0 = min(ydata)
        yn = max(ydata)
        t = self.unselected[0,:]
        selected = (t >= t0) & (t <= tn) & (y0 <= self.unselected[1,:]) & (self.unselected[1,:] <= yn)
        indicies = [i for i, v in enumerate(selected) if v]

        print indicies
        sel = self.unselected[0:3,indicies]
        self.selected = numpy.append(self.selected, sel, 1)

        self.UpdatePlot()

    def OnPick(self, event):
        if event.mouseevent.button != 1:
            return

        tolerance = 5
        thisline = event.artist
        xdata, ydata = thisline.get_data()
        ind = event.ind

        sel = self.unselected[0:3, ind]
        self.selected = numpy.append(self.selected, sel, 1)

        self.UpdatePlot()

    def UpdatePlot(self):
        ax = self.gca()

        self.selected_points.set_data(self.selected[0:2,:])

        self.canvas.figure.draw_artist(self.unselected_points)
        self.canvas.figure.draw_artist(self.selected_points)
        self.canvas.blit(ax.bbox)
        print "flagged obs_raw_ids", self.selected[2,:]

    def OnKeyPress(self, event):
        print "key pressed", event.key, event.xdata, event.ydata
        if event.key == 'q':
            sys.exit(0)

        if event.key == 'F':
            con = psycopg2.connect(host='localhost', database='crmp', user='httpd', sslmode='prefer')
            cur = con.cursor()
            flag_it_up(cur, self.selected[2,:], 'manual')
            cur.close()
            con.commit()

        if event.key == 'C':
            self.selected = numpy.empty((3,0))
            self.UpdatePlot()

class MyTimeseries:
    def __init__(self, station_id, variable):
        self.station_id = station_id
        self.variable = variable

        con = psycopg2.connect(host='localhost', database='crmp', user='httpd', sslmode='prefer')
        cur = con.cursor()
        select = "SELECT obs_time, datum, obs_raw_id FROM meta_vars NATURAL JOIN obs_raw NATURAL JOIN meta_history NATURAL JOIN meta_station WHERE station_id = %d AND net_var_name = '%s' ORDER BY obs_time" % (station_id, variable)
        print select
        cur.execute(select)
        data = cur.fetchall()
        data = numpy.array(data)
        print data
        print data.shape
        self.data = data

def flag_it_up(cur, obs_ids, flag_name):
    cur.execute("SELECT pcic_flag_id FROM meta_pcic_flag WHERE flag_name = %s", (flag_name,))
    fid = cur.fetchall()[0][0]

    cur.execute("SELECT obs_raw_id FROM obs_raw_pcic_flags WHERE pcic_flag_id = %s", (fid,))
    dont_insert = cur.fetchall()
    obs_ids = numpy.array(list(set(obs_ids).difference(map(lambda x: x[0], dont_insert))))

    data = numpy.array([obs_ids, numpy.repeat(fid, len(obs_ids))]).transpose()
    try:
        cur.executemany("INSERT INTO obs_raw_pcic_flags (obs_raw_id, pcic_flag_id) VALUES (%s, %s)", data)
        print "Inserted %d rows" % cur.rowcount
    except Exception, e:
        print cur.query
        print e


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option('-s', '--station_id', dest='station_id', type=int)
    parser.add_option('-v', '--variable', dest='variable')
    parser.set_defaults(station_id='1002', variable='temperature')

    opts, args = parser.parse_args()
    print opts

    ts = MyTimeseries(opts.station_id, opts.variable)

    fig = plt.figure(FigureClass=MyFigure)
    fig.PostInit(ts)
    plt.show()
