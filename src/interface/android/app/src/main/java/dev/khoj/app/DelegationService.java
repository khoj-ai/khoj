package dev.khoj.app;


import com.google.androidbrowserhelper.locationdelegation.LocationDelegationExtraCommandHandler;


public class DelegationService extends
        com.google.androidbrowserhelper.trusted.DelegationService {
    @Override
    public void onCreate() {
        super.onCreate();


            registerExtraCommandHandler(new LocationDelegationExtraCommandHandler());

    }
}
