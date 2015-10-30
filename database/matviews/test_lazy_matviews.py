from pycds import *
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy import and_, or_

if __name__ == '__main__':
    '''
    Perform a couple inserts/updates/deletes to make sure the triggers don't cause errors
    '''
    connection_string='postgresql://bveerman@medusa.pcic/crmp?sslmode=require'

    engine = create_engine(connection_string, echo=False)
    Session = sessionmaker(bind=engine)
    session = Session()
    # All matviews depend obs_raw and/or meta_history, so we only need to insert, delete, and update these tables

    session.begin(subtransactions=True)

    # test inserts
    stn = Station(native_id='teststation', network_id=1)
    session.add(stn)
    session.flush()

    my_stn = session.query(Station).filter_by(native_id='teststation').first()
    hist = History(station_id=my_stn.id, station_name='teststation')
    session.add(hist)
    session.flush()
    print 'Should now have one matview_changes entry for station_obs_stats'
    print session.execute('SELECT distinct id, mv_name from matviews m inner join matview_changes mc on (m.id = mc.mv_id)').fetchall()

    var = Variable(network_id=1, name='testvarnetname', standard_name='testvar')
    session.add(var)
    session.flush()
    my_var = session.query(Variable).filter_by(name='testvarnetname').first()
    print 'Should now have 2 matview_changes: station_obs_stats and climo_obs_count'
    print session.execute('SELECT distinct id, mv_name from matviews m inner join matview_changes mc on (m.id = mc.mv_id)').fetchall()

    my_hist = session.query(History).filter_by(station_id=my_stn.id).first()
    ob = Obs(time='2099-01-01 00:00:00', datum=10, vars_id=my_var.id, history_id=my_hist.id)
    session.add(ob)
    session.flush()
    print 'Should now have 4 entries for all lazy matviews'
    print session.execute('SELECT distinct id, mv_name from matviews m inner join matview_changes mc on (m.id = mc.mv_id)').fetchall()
    my_ob = session.query(Obs).filter_by(history_id=my_hist.id).first()



    # test updates
    print 'Testing updates... '
    my_stn.native_id = 'teststation-newnative-id'
    session.flush()
    print 'my_stn',
    my_var.standard_name = 'testvarname-new'
    session.flush()
    print 'my_var',
    my_hist.station_name = 'teststation-newname'
    session.flush()
    print 'my_hist',
    my_ob.datum = 20
    session.flush()
    print 'my_ob'

    # test deletes
    print 'Testing deletes... '
    session.delete(my_ob)
    session.flush()
    print 'my_ob',
    # session.execute('DELETE FROM meta_vars WHERE vars_id = {}'.format(my_var.id))
    session.delete(my_var) #This kinda takes forever... haven't looked into why and running the pure sql doesn't help
    session.flush()
    print 'my_var',
    session.delete(my_hist)
    session.flush()
    print 'my_hist',
    session.delete(my_stn)
    session.flush()
    print 'my_stn'



    # lets not actually do any of that...
    session.rollback()
    