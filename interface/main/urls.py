from django.urls import path
from . import views
from .dash_apps import home, visuals

urlpatterns = [
    path('', views.index, name="index"),
    path('profile', views.profile, name="profile"),
    path('visuals', views.visuals, name="visuals")
]