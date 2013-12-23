#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <sstream>
#include <stdint.h>
#include <string>
#include <vector>

#include <btBulletDynamicsCommon.h>
using namespace std;

int main(int argc, char **argv)
{
  btBroadphaseInterface* broadphase = new btDbvtBroadphase();

  btDefaultCollisionConfiguration* collisionConfiguration = new btDefaultCollisionConfiguration();
  btCollisionDispatcher* dispatcher = new btCollisionDispatcher(collisionConfiguration);

  btSequentialImpulseConstraintSolver* solver = new btSequentialImpulseConstraintSolver;

  btDiscreteDynamicsWorld* dynamicsWorld = new btDiscreteDynamicsWorld(dispatcher,broadphase,solver,collisionConfiguration);
  dynamicsWorld->setGravity(btVector3(0,-10,0));

  btCollisionShape* groundShape = new btStaticPlaneShape(btVector3(1,1,0),1);

  btCollisionShape* fallShape = new btSphereShape(1);

  btDefaultMotionState* groundMotionState = 
    new btDefaultMotionState(btTransform(btQuaternion(0,0,0,1),btVector3(0,-1,0)));

  btRigidBody::btRigidBodyConstructionInfo
    groundRigidBodyCI(0,groundMotionState,groundShape,btVector3(0,0,0));
  btRigidBody* groundRigidBody = new btRigidBody(groundRigidBodyCI);
  dynamicsWorld->addRigidBody(groundRigidBody);


  btDefaultMotionState* fallMotionState =
    new btDefaultMotionState(btTransform(btQuaternion(0,0,0,1),btVector3(.5,100,0)));
  btScalar mass = 1;
  btVector3 fallInertia(0,0,0);
  fallShape->calculateLocalInertia(mass,fallInertia);
  btRigidBody::btRigidBodyConstructionInfo fallRigidBodyCI(mass,fallMotionState,fallShape,fallInertia);
  btRigidBody* fallRigidBody = new btRigidBody(fallRigidBodyCI);
  dynamicsWorld->addRigidBody(fallRigidBody);

  btDefaultMotionState* ball2Motion =
    new btDefaultMotionState(btTransform(btQuaternion(0,0,0,1),btVector3(0,20,0)));
  btScalar mass2 = 1;
  btRigidBody::btRigidBodyConstructionInfo ball2CI(mass,ball2Motion,fallShape,fallInertia);
  btRigidBody* ball2Body = new btRigidBody(ball2CI);
  dynamicsWorld->addRigidBody(ball2Body);

  for (int i=0 ; i<1000 ; i++) {
    dynamicsWorld->stepSimulation(1/60.f,10);

    btTransform trans;
    fallRigidBody->getMotionState()->getWorldTransform(trans);

    std::cout << trans.getOrigin().getY() << std::endl;
  }

  dynamicsWorld->removeRigidBody(ball2Body);
  delete ball2Body->getMotionState();
  delete ball2Body;

  dynamicsWorld->removeRigidBody(fallRigidBody);
  delete fallRigidBody->getMotionState();
  delete fallRigidBody;

  dynamicsWorld->removeRigidBody(groundRigidBody);
  delete groundRigidBody->getMotionState();
  delete groundRigidBody;


  delete fallShape;

  delete groundShape;

  delete dynamicsWorld;
  delete solver;
  delete dispatcher;
  delete collisionConfiguration;
  delete broadphase;

  return 0;
}
